import ackcord._
import ackcord.commands.PrefixParser
import ackcord.gateway.GatewayIntents
import ackcord.syntax._

object Main extends App {

    val GeneralCommands = "!"

    require(args.nonEmpty, "Token?")

    val token = args.head

    val settings = ClientSettings(token, intents = GatewayIntents.AllNonPrivileged)
    import settings.executionContext

    settings.createClient().foreach { client =>
        client.onEventSideEffectsIgnore {
            case APIMessage.Ready(_) => println("Now ready")
        }



        val myEvents      = new MyEvents(client.requests)
        val myListeners   = new Listeners(client)
        val myCommands    = new MyCommands(client, client.requests)
        val myHelpCommand = new MyHelpCommand(client.requests)

        client.bulkRegisterListeners(
            myEvents.printReady,
            myEvents.welcomeNew
        )

        client.registerListener(myListeners.createListeners)

        client.commands.runNewCommand(
            PrefixParser.structured(needsMention = true, Seq("!"), Seq("help")),
            myHelpCommand.command
        )

        client.commands.bulkRunNamedWithHelp(
            myHelpCommand,
            myCommands.hello,
            myCommands.setShouldMention,
            myCommands.guildInfo,
            myCommands.timeDiff,
            myCommands.ping,
            myCommands.maybeFail,
            myCommands.ratelimitTest("ratelimitTest", client.requests.sinkIgnore[Any]),
            myCommands
                    .ratelimitTest("ratelimitTestOrdered", client.requests.sinkIgnore[Any](Requests.RequestProperties.ordered)),
            myCommands.kill
        )

        client.login()

    }

}
