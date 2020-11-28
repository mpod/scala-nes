package scalanes.mutable
import java.awt._
import java.awt.event.{KeyEvent, KeyListener, WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import java.nio.file.Path

import cats.effect.{Effect, ExitCode, IO, IOApp}
import fs2.Stream
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.DurationInt
import scala.language.higherKinds

class UI[F[_]](buttons: SignallingRef[F, Int], interrupter: SignallingRef[F, Boolean])(implicit F: Effect[F]) {
  def start: Stream[F, Array[Int] => Unit] =
    Stream.eval(F.delay {
      val width          = 2 * 256
      val height         = 2 * 240
      var bufferedImageA = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      var bufferedImageB = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      val canvas: Canvas = new Canvas {
        setSize(width, height)
        setBackground(Color.WHITE)
        override def paint(g: Graphics): Unit  = render(g)
        override def update(g: Graphics): Unit = render(g)
        private def render(g: Graphics): Unit  = g.asInstanceOf[Graphics2D].drawImage(bufferedImageA, 0, 0, null)
      }
      val frame = new Frame("ScalaNES Console")
      frame.setSize(width, height)
      frame.setLayout(new BorderLayout())
      frame.add(canvas)
      frame.addWindowListener(new WindowAdapter() {
        override def windowClosing(we: WindowEvent): Unit = {
          F.runAsync(interrupter.set(true))(_ => IO.unit).unsafeRunSync()
          frame.dispose()
        }
      })
      frame.addKeyListener(new KeyListener {
        override def keyTyped(keyEvent: KeyEvent): Unit = ()

        override def keyPressed(keyEvent: KeyEvent): Unit =
          F.runAsync {
            keyEvent.getKeyCode match {
              case KeyEvent.VK_X =>
                buttons.modify(x => (x | 0x80, x)) // A
              case KeyEvent.VK_Z =>
                buttons.modify(x => (x | 0x40, x)) // B
              case KeyEvent.VK_A =>
                buttons.modify(x => (x | 0x20, x)) // Select
              case KeyEvent.VK_S =>
                buttons.modify(x => (x | 0x10, x)) // Start
              case KeyEvent.VK_UP =>
                buttons.modify(x => (x | 0x08, x)) // Up
              case KeyEvent.VK_DOWN =>
                buttons.modify(x => (x | 0x04, x)) // Down
              case KeyEvent.VK_LEFT =>
                buttons.modify(x => (x | 0x02, x)) // Left
              case KeyEvent.VK_RIGHT =>
                buttons.modify(x => (x | 0x01, x)) // Right
              case _ =>
                F.pure(0)
            }
          }(_ => IO.unit)
            .unsafeRunSync()

        override def keyReleased(keyEvent: KeyEvent): Unit =
          F.runAsync {
            keyEvent.getKeyCode match {
              case KeyEvent.VK_X =>
                buttons.modify(x => (x & ~0x80, x)) // A
              case KeyEvent.VK_Z =>
                buttons.modify(x => (x & ~0x40, x)) // B
              case KeyEvent.VK_A =>
                buttons.modify(x => (x & ~0x20, x)) // Select
              case KeyEvent.VK_S =>
                buttons.modify(x => (x & ~0x10, x)) // Start
              case KeyEvent.VK_UP =>
                buttons.modify(x => (x & ~0x08, x)) // Up
              case KeyEvent.VK_DOWN =>
                buttons.modify(x => (x & ~0x04, x)) // Down
              case KeyEvent.VK_LEFT =>
                buttons.modify(x => (x & ~0x02, x)) // Left
              case KeyEvent.VK_RIGHT =>
                buttons.modify(x => (x & ~0x01, x)) // Right
              case _ =>
                F.pure(0)
            }
          }(_ => IO.unit)
            .unsafeRunSync()
      })
      frame.setVisible(true)
      var frameStart = System.currentTimeMillis()

      (rgbs: Array[Int]) => {
        bufferedImageB.setRGB(0, 0, width, height, rgbs, 0, width)
        val bufferedImageC = bufferedImageA
        bufferedImageA = bufferedImageB
        bufferedImageB = bufferedImageC
        canvas.repaint()
        val diff = System.currentTimeMillis() - frameStart
        println(s"Frame generated in $diff ms")
        frameStart = System.currentTimeMillis()
      }
    })
}

object Console extends IOApp {
  override def run(args: scala.List[String]): IO[ExitCode] = {
    val stream: Stream[IO, Unit] = for {
      buttons     <- Stream.eval(SignallingRef[IO, Int](0))
      interrupter <- Stream.eval(SignallingRef[IO, Boolean](false))
      imagePath = args.head
      ui        = new UI[IO](buttons, interrupter)
      updateCanvas <- ui.start
      _ <- NesState
        .fromFile[IO](Path.of(imagePath))
        .head
        .map(NesState.reset)
        .flatMap { initial =>
          Stream.unfoldEval(initial) { nes =>
            buttons.get
              .map(b => NesState.setButtons(b)(nes))
              .map { nes =>
                updateCanvas(nes.ppuState.canvas)
                val next = NesState.executeFrame(nes)
                Option(next, next)
              }
          }
        }
        .metered(16.milliseconds)
        .interruptWhen(interrupter)
    } yield ()
    stream.compile.drain.as(ExitCode.Success)
  }
}
