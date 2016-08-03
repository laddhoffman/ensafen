## Description
*ensafen* is a reverse proxy that performs filtering to prevent SQL injection attacks.
It is built as a programming exercise, and is not recommended for use in production at this time.

## Features
* A configuration file is used to specify the destination host. This is done in a URI format,
similar to what you might use in a reverse proxy like [nginx|nginx.org], which offers a degree
of flexibility in configuring the destination. The destination can be an http or https address;
the port can be specified; and a remote path can be specified, to which the requset path will be
appended. For example, if your destination is `https://yourdomain.com/somedir`, and *ensafen* is
listening on localhost port 8080, then a request to `localhost:8080/file.ext` will be forwarded to
`https://yourdomain.com/somedir/file.ext`. Parsing of these parameters is intended to "just work",
but of course testing is needed and your mileage may vary.
* *ensafen* is stateless, in the sense that it pays no regard to a client's past behavior.
Each request is filtered on its own merits.

## Usage
1. Clone the repository

 `git clone https://github.com/laddhoffman/ensafen`

2. Enter the project directory

 `cd ensafen`

3. Optionally, edit the configuration file config/sys.config

4. Build the project

 `make`

5. Execute the application

  * With console

   `./_rel/ensafen_release/bin/ensafen_release console`

  * As a daemon

    * Start

    `./_rel/ensafen_release/bin/ensafen_release start`

    * Stop

    `./_rel/ensafen_release/bin/ensafen_release stop`

6. Perform any web request to the reverse proxy and it will pass your request
 to the destination host, and pass the reply back to you. Unless you attempt a
 SQL injection attack; then, you should receive a 404 response from *ensafen*.

7. To override the `sys.config` without rebuilding the application, place a
 `sys.config` file in the directory of the release - in this case,
 `./_rel/ensafen_release/`.

## Ideas for Improvement
* Forward chunks as they arrive, rather than buffering the entire request/response
* Write unit tests for SQL injections and valid inputs
  * The unit test should launch its own HTTP service, and act as both the
 reverse proxy client and destination host
* Profile *ensafen* to identify performance bottlenecks

## Dependencies and Citations
This project relies on three projects from [99s|ninenines.eu]: cowboy, gun, and
erlang.mk. Cowboy is used to talk to our application's client via HTTP, to receive
their request and send our reply. Gun is used to issue a request to the destination
host, on behalf of our client. Erlang.mk is a build system and package manager.

[httpbin.org] provides a useful endpoint to which we can issue test HTTP requests.

The regular expressions provided in this package are based on [this guide|http://www.symantec.com/connect/articles/detection-sql-injection-and-cross-site-scripting-attacks] presented by [Symantec|symantec.com].

