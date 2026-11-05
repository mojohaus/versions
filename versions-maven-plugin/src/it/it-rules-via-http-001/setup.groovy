import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpHandler
import com.sun.net.httpserver.HttpServer

import java.util.concurrent.Executors

// Serves rule-set.xml over HTTP, behind Basic authentication, and records every request in requests.log
String expected = 'Basic ' + 'it-user:it-secret'.bytes.encodeBase64().toString()
File rules = new File(basedir, 'rule-set.xml')
File requests = new File(basedir, 'requests.log')
requests.text = ''

HttpServer server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0)
server.executor = Executors.newCachedThreadPool({ Runnable r ->
    Thread t = new Thread(r)
    t.daemon = true
    t
})
server.createContext('/rules/rule-set.xml', { HttpExchange exchange ->
    boolean authenticated = exchange.requestHeaders.getFirst('Authorization') == expected
    if (authenticated) {
        byte[] body = rules.bytes
        exchange.responseHeaders.add('Content-Type', 'application/xml')
        exchange.sendResponseHeaders(200, body.length)
        exchange.responseBody.write(body)
    } else {
        exchange.responseHeaders.add('WWW-Authenticate', 'Basic realm="rules"')
        exchange.sendResponseHeaders(401, -1)
    }
    exchange.close()
    requests << "${exchange.requestMethod} ${exchange.responseCode} ${authenticated ? 'authenticated' : 'anonymous-or-wrong'}\n"
} as HttpHandler)
server.start()
context.put('rules.server', server)

File pom = new File(basedir, 'pom.xml')
pom.setText(pom.getText('UTF-8').replace('__PORT__', String.valueOf(server.address.port)), 'UTF-8')

return true
