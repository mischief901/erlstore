# Store

To start a new master server, from the top level erlstore folder, run "python master.py".

To start a new file server or backup node, from the top level erlstore folder, run "python file.py [servernumber]".

To access elixir client, once master node is up, visit 127.0.0.1:4000. Server IP is 127.0.0.1. If xclip is installed, starting client server will copy sctp port to clipboard for easy connection using the elixir front end. After the Master Server fails and the backup takes over, the new sctp port has to be re-entered as well.

Currently set for easy localhost startup. Configuration file changes can be made to run across a network.

