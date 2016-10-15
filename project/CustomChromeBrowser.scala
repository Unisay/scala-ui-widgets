import org.openqa.selenium.chrome.{ChromeDriverService, ChromeOptions}
import org.openqa.selenium.remote.RemoteWebDriver
import org.scalajs.core.tools.io.{MemVirtualJSFile, VirtualJSFile}
import org.scalajs.jsenv.selenium.{BrowserDriver, SeleniumBrowser}

class CustomChromeBrowser(val customJs: String) extends SeleniumBrowser {

  def name: String = "Chrome"
  def newDriver: BrowserDriver = new ChromeDriver

  private class ChromeDriver extends BrowserDriver {
    protected def newDriver(): RemoteWebDriver = {
      val service = {
        /* Activate the silent ChromeDriverService silent mode,
         * see ChromeDriverService.createDefaultService
         */
        new ChromeDriverService.Builder().withSilent(true).usingAnyFreePort.build
      }
      new org.openqa.selenium.chrome.ChromeDriver(service, new ChromeOptions)
    }
  }

  override def initFiles(): Seq[VirtualJSFile] = {
    super.initFiles() :+ new MemVirtualJSFile("setupCustomCode.js").withContent(s"(function() { $customJs })()")
  }
}
