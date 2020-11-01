import ackcord._
import ackcord.syntax._
import akka.NotUsed

class MyEvents(requests: Requests) extends EventsController(requests) {

    val printReady: EventListener[APIMessage.Ready, NotUsed] =
        Event.on[APIMessage.Ready].withSideEffects(_ => println("Now ready from controller"))

    val welcomeNew: EventListener[APIMessage.GuildMemberAdd, NotUsed] =
        GuildUserEvent.on[APIMessage.GuildMemberAdd].withRequestOpt { r =>
            for {
                systemChannel <- r.guild.systemChannel
            } yield systemChannel.sendMessage(s"Welcome to the club ${r.user.username}, *slap* *ha~aegh*")
        }
}