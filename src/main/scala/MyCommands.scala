import java.time.temporal.ChronoUnit

import ackcord._
import ackcord.commands._
import ackcord.data.{GuildId, Permission}
import ackcord.requests.{CreateMessage, Request}
import ackcord.syntax._
import akka.NotUsed
import akka.stream.scaladsl.{Flow, Sink}
import cats.syntax.all._
import com.sedmelluq.discord.lavaplayer.player.{AudioPlayerManager, DefaultAudioPlayerManager}
import com.sedmelluq.discord.lavaplayer.source.AudioSourceManagers
import com.sedmelluq.discord.lavaplayer.track.{AudioPlaylist, AudioTrack}

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.Random

class MyCommands(client: DiscordClient, requests: Requests) extends CommandController(requests) {

    val hello: NamedDescribedCommand[NotUsed] =
        Command
                .named(Seq("!"), Seq("hello"), mustMention = true) //Simplest way to name a command
                .described("Hello", "Say hello")
                .withRequest(m => m.textChannel.sendMessage(s"Hello ${m.user.username}"))

    //probably store it in a database instead
    val shouldMentionMap = new TrieMap[GuildId, Boolean]
    val prefixSymbolsMap = new TrieMap[GuildId, Seq[String]]

    def needMentionInGuild(guildId: GuildId): Future[Boolean] =
        Future.successful(shouldMentionMap.getOrElseUpdate(guildId, false))

    def prefixSymbolsInGuild(guildId: GuildId): Future[Seq[String]] =
        Future.successful(prefixSymbolsMap.getOrElseUpdate(guildId, Seq("m!")))

    def dynamicPrefix(aliases: String*): StructuredPrefixParser =
        PrefixParser.structuredAsync(
            (c, m) => m.guild(c).fold(Future.successful(false))(g => needMentionInGuild(g.id)),
            (c, m) => m.guild(c).fold(Future.successful(Seq("m!")))(g => prefixSymbolsInGuild(g.id)),
            (_, _) => Future.successful(aliases)
        )

    val setShouldMention: NamedDescribedCommand[Boolean] =
        GuildCommand
                .namedParser(dynamicPrefix("setShouldMention"))
                .described("Set should mention", "Set if commands need a mention of the bot before the prefix")
                .parsing[Boolean]
                .withRequest { m =>
                    shouldMentionMap.put(m.guild.id, m.parsed)
                    m.textChannel.sendMessage(s"Set should mention to ${m.parsed}")
                }


    val guildInfo: NamedDescribedCommand[NotUsed] =
        GuildCommand
                .namedParser(dynamicPrefix("guildInfo"))
                .described("Guild info", "Prints info about the current guild")
                .withRequest { m =>
                    val guildName   = m.guild.name
                    val channelName = m.textChannel.name
                    val userNick    = m.guildMember.nick.getOrElse(m.user.username)

                    m.textChannel.sendMessage(
                        s"This guild is named $guildName, the channel is named $channelName and you are called $userNick"
                    )
                }

    val parsingNumbers: NamedDescribedCommand[(Int, Int)] =
        Command
                .namedParser(dynamicPrefix("parseNum"))
                .described("Parse numbers", "Have the bot parse two numbers")
                .parsing((MessageParser[Int], MessageParser[Int]).tupled)
                .withRequest(m => m.textChannel.sendMessage(s"Arg 1: ${m.parsed._1}, Arg 2: ${m.parsed._2}"))


    private val ElevatedCommand: CommandBuilder[GuildUserCommandMessage, NotUsed] =
        GuildCommand.andThen(CommandBuilder.needPermission[GuildUserCommandMessage](Permission.Administrator))


    val timeDiff: NamedDescribedCommand[NotUsed] =
        Command
                .namedParser(dynamicPrefix("timeDiff"))
                .described("Time diff", "Checks the time between sending and seeing a message")
                .asyncOpt { implicit m =>
                    import requestHelper._
                    for {
                        sentMsg <- run(m.textChannel.sendMessage("Msg"))
                        time = ChronoUnit.MICROS.between(m.message.timestamp, sentMsg.timestamp)
                        _ <- run(m.textChannel.sendMessage(s"$time ms between command and response"))
                    } yield ()
                }

    val ping: NamedDescribedCommand[NotUsed] =
        Command.namedParser(dynamicPrefix("ping")).described("Ping", "Checks if the bot is alive").toSink {
            Flow[CommandMessage[NotUsed]]
                    .map(m => CreateMessage.mkContent(m.message.channelId, "Pong"))
                    .to(requests.sinkIgnore)
        }

    def ratelimitTest(name: String, sink: Sink[Request[_], _]): NamedDescribedCommand[Int] =
        Command
                .namedParser(dynamicPrefix(name))
                .described("Ratelimit test", "Checks that ratelimiting is working as intended")
                .parsing[Int]
                .toSink {
                    Flow[CommandMessage[Int]]
                            .mapConcat(implicit m => List.tabulate(m.parsed)(i => m.textChannel.sendMessage(s"Msg$i")))
                            .to(sink)
                }

    val maybeFail: NamedDescribedCommand[NotUsed] = Command
            .namedParser(dynamicPrefix("maybeFail"))
            .described("MaybeFail", "A command that sometimes fails and throws an exception")
            .withRequest { r =>
                if (Random.nextInt(100) < 25) {
                    throw new Exception("Failed")
                }

                r.textChannel.sendMessage("Succeeded")
            }

    val kill: NamedDescribedCommand[NotUsed] =
        ElevatedCommand
                .namedParser(dynamicPrefix("kill", "die"))
                .described("Kill", "Kills the bot")
                .withSideEffects(_ => client.shutdownJVM())

    val playerManager: AudioPlayerManager = new DefaultAudioPlayerManager
    AudioSourceManagers.registerRemoteSources(playerManager)

    val queue: NamedCommand[String] =
        GuildVoiceCommand.namedParser(dynamicPrefix("queue", "q")).parsing[String].streamed { r =>
            val guildId     = r.guild.id
            val url         = r.parsed
            val loadItem    = client.loadTrack(playerManager, url)
            val joinChannel = client.joinChannel(guildId, r.voiceChannel.id, playerManager.createPlayer())

            loadItem.zip(joinChannel).map {
                case (track: AudioTrack, player) =>
                    player.startTrack(track, true)
                    client.setPlaying(guildId, playing = true)
                case (playlist: AudioPlaylist, player) =>
                    if (playlist.getSelectedTrack != null) {
                        player.startTrack(playlist.getSelectedTrack, false)
                    } else {
                        player.startTrack(playlist.getTracks.get(0), false)
                    }
                    client.setPlaying(guildId, playing = true)
                case _ => sys.error("Unknown audio item")
            }
        }

}