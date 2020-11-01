import ackcord._
import ackcord.data._
import akka.NotUsed

class Listeners(client: DiscordClient) extends EventsController(client.requests) {

    val MessageEvent: EventListenerBuilder[TextChannelEventListenerMessage, APIMessage.MessageCreate] =
        TextChannelEvent.on[APIMessage.MessageCreate]

    def listen(inChannel: TextChannelId, identifier: String): EventListener[APIMessage.MessageCreate, NotUsed] =
        MessageEvent.withSideEffects { m =>
            if (m.channel.id == inChannel) {
                println(s"$identifier: ${m.event.message.content}")
            }
        }

    def stopListen(
                          inChannel: TextChannelId,
                          identifier: String,
                          listener: EventRegistration[NotUsed],
                          stopper: EventRegistration[NotUsed]
                  ): EventListener[APIMessage.MessageCreate, NotUsed] =
        MessageEvent.withSideEffects { m =>
            if (m.channel.id == inChannel && m.event.message.content == "stop listen " + identifier) {
                listener.stop()
                stopper.stop()
            }
        }

    val createListeners: EventListener[APIMessage.MessageCreate, NotUsed] =
        MessageEvent.withSideEffects { m =>
            val startMessage = m.event.message

            if (startMessage.content.startsWith("start listen ")) {
                val identifier = startMessage.content.replaceFirst("start listen ", "")

                val listener = client.registerListener(listen(startMessage.channelId, identifier))

                lazy val stopper: EventRegistration[NotUsed] =
                    client.registerListener(stopListen(startMessage.channelId, identifier, listener, stopper))

                //Initialize stopper
                stopper
            }
        }
}
