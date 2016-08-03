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
 SQL injection attack; then, you may receive an error message, or you may
 receive no reply.

7. To override the `sys.config` without rebuilding the application, place a
 `sys.config` file in the directory of the release - in this case,
 `./_rel/ensafen_release/`.

## Ideas for Improvement
* Forward chunks as they arrive, rather than buffering the entire request/response


