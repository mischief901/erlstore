"""

Tufts COMP 112 - Networks
Spring 2018
Project 1
Erlstore

Name: Alex Misch
Email: Alexander.Misch@tufts.edu
UTLN: amisch01

Name: Matt Turner
Email: Matthew.Turner@tufts.edu
UTLN: mturne07

master.py

Creates necessary bookkeeping files for instantiation of a fileserver
using erlstore release published via mix. Automatically starts the server
on completion.

Invoked via command line.

Usage: python2 master.py <name> <Master_IP> <Master_Port>

"""



import sys
import os
import subprocess



def generate_files(input_list):
    """
    generate_files(input_list):

    Creates necessary bookkeeping files for instantiation of a master server
    using erlstore release published via mix. Automatically starts the server
    on completion.

    Input: The input args
    Output: A running master server

    """

    Name = input_list[1]
    IP = input_list[2]
    Port = input_list[3]

    configfile = open('./releases/0.1.0/sys.config', 'w')

    configfile.write("[{sasl,[{errlog_type,error}]}," + "\n" +
    "{kernel, [{inet_default_listen_options, " + "\n" +
    "[{reuseaddr, true}]}]}," + "\n" +
    "{interface," + "\n" +
    "[{'Elixir.InterfaceWeb.Endpoint'," + "\n" +
    "[{url,[{host,<<\"localhost\">>}]}," + "\n" +
    "{server,true}," + "\n" +
    "{secret_key_base," + "\n" +
    "<<\"/K+vEuq1qfeCWL3XkQ4UJZl5/Vz5++uU+eCvzd47uBOUST8elCl6im4bC7nBZHa/\">>}," + "\n" +
    "{render_errors," + "\n" +
    "[{view,'Elixir.InterfaceWeb.ErrorView'}," + "\n" +
    "{accepts,[<<\"html\">>,<<\"json\">>]}]}," + "\n" +
    "{pubsub," + "\n" +
    "[{name,'Elixir.Interface.PubSub'}," + "\n" +
    "{adapter,'Elixir.Phoenix.PubSub.PG2'}]}," + "\n" +
    "{http,[{port,4000}, {ip, {127,0,0,1}}, {reuseaddr, true}]}," + "\n" +
    "{debug_errors,true}," + "\n" +
    "{code_reloader,false}," + "\n" +
    "{check_origin,false}," + "\n" +
    "{watchers,[]}," + "\n" +
    "{live_reload," + "\n" +
    "[{patterns," + "\n" +
    "[#{'__struct__' => 'Elixir.Regex',opts => <<>>," + "\n" +
    "re_pattern =>" + "\n" +
    "{re_pattern,1,0,0," + "\n" +
    "<<69,82,67,80,166,0,0,0,0,0,0,0,81,0,0,0,255," + "\n" +
    "255,255,255,255,255,255,255,112,0,47,0,0,0,1," + "\n" +
    "0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0," + "\n" +
    "0,0,0,0,0,0,0,0,0,0,0,131,0,98,29,112,29,114," + "\n" +
    "29,105,29,118,29,47,29,115,29,116,29,97,29," + "\n" +
    "116,29,105,29,99,29,47,85,12,133,0,9,0,1,29," + "\n" +
    "106,29,115,119,0,9,29,99,29,115,29,115,119,0," + "\n" +
    "9,29,112,29,110,29,103,119,0,11,29,106,29," + "\n" +
    "112,29,101,29,103,119,0,9,29,106,29,112,29," + "\n" +
    "103,119,0,9,29,103,29,105,29,102,119,0,9,29," + "\n" +
    "115,29,118,29,103,120,0,65,25,120,0,98,0>>}," + "\n" +
    "re_version => <<\"8.41 2017-07-05\">>," + "\n" +
    "source =>" + "\n" +
    "<<\"priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$\">>}," + "\n" +
    "#{'__struct__' => 'Elixir.Regex',opts => <<>>," + "\n" +
    "re_pattern =>" + "\n" +
    "{re_pattern,1,0,0," + "\n" +
    "<<69,82,67,80,112,0,0,0,0,0,0,0,81,0,0,0,255," + "\n" +
    "255,255,255,255,255,255,255,112,0,111,0,0,0," + "\n" +
    "1,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0," + "\n" +
    "0,0,0,0,0,0,0,0,0,0,0,0,131,0,44,29,112,29," + "\n" +
    "114,29,105,29,118,29,47,29,103,29,101,29,116," + "\n" +
    "29,116,29,101,29,120,29,116,29,47,85,12,133," + "\n" +
    "0,9,0,1,29,112,29,111,120,0,9,25,120,0,44,0>>}," + "\n" +
    "re_version => <<\"8.41 2017-07-05\">>," + "\n" +
    "source => <<\"priv/gettext/.*(po)$\">>}," + "\n" +
    "#{'__struct__' => 'Elixir.Regex',opts => <<>>," + "\n" +
    "re_pattern =>" + "\n" +
    "{re_pattern,1,0,0," + "\n" +
    "<<69,82,67,80,134,0,0,0,0,0,0,0,81,0,0,0,255," + "\n" +
    "255,255,255,255,255,255,255,108,0,120,0,0,0," + "\n" +
    "1,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0," + "\n" +
    "0,0,0,0,0,0,0,0,0,0,0,0,131,0,66,29,108,29," + "\n" +
    "105,29,98,29,47,29,105,29,110,29,116,29,101," + "\n" +
    "29,114,29,102,29,97,29,99,29,101,29,95,29," + "\n" +
    "119,29,101,29,98,29,47,29,118,29,105,29,101," + "\n" +
    "29,119,29,115,29,47,85,12,133,0,9,0,1,29,101," + "\n" +
    "29,120,120,0,9,25,120,0,66,0>>}," + "\n" +
    "re_version => <<\"8.41 2017-07-05\">>," + "\n" +
    "source => <<\"lib/interface_web/views/.*(ex)$\">>}," + "\n" +
    "#{'__struct__' => 'Elixir.Regex',opts => <<>>," + "\n" +
    "re_pattern =>" + "\n" +
    "{re_pattern,1,0,0," + "\n" +
    "<<69,82,67,80,144,0,0,0,0,0,0,0,81,0,0,0,255," + "\n" +
    "255,255,255,255,255,255,255,108,0,120,0,0,0," + "\n" +
    "1,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0," + "\n" +
    "0,0,0,0,0,0,0,0,0,0,0,0,131,0,76,29,108,29," + "\n" +
    "105,29,98,29,47,29,105,29,110,29,116,29,101," + "\n" +
    "29,114,29,102,29,97,29,99,29,101,29,95,29," + "\n" +
    "119,29,101,29,98,29,47,29,116,29,101,29,109," + "\n" +
    "29,112,29,108,29,97,29,116,29,101,29,115,29," + "\n" +
    "47,85,12,133,0,11,0,1,29,101,29,101,29,120," + "\n" +
    "120,0,11,25,120,0,76,0>>}," + "\n" +
    "re_version => <<\"8.41 2017-07-05\">>," + "\n" +
    "source =>" + "\n" +
    "<<\"lib/interface_web/templates/.*(eex)$\">>}]}]}]}]}," + "\n" +
    "{logger," + "\n" +
    "[{console,[{metadata,[user_id]},{format,<<\"[$level] $message\n\">>}]}]}," + "\n" +
    "{phoenix,[{stacktrace_depth,20}]}," + "\n" +
                     "{connect,[{master,true},{default_port,\"%d\"}]}]." % (int(Port)))
    configfile.close()


    vmfile = open('./releases/0.1.0/vm.args', 'w')
    vmfile.write("## Name of the node" + "\n" +
    "-name \"{0}\"@\"{1}\"".format(str(Name), str(IP)) + "\n" +
    "## Cookie for distributed erlang" + "\n" +
    "-setcookie =7@;%|GJju=_`yNMRYz9N:9?|aG;{T*)[AcN$5YE{G36`UXlYV(BJFVy120JrA}k" + "\n" +
    "## Heartbeat management; auto-restarts VM if it dies or becomes unresponsive" + "\n" +
    "## (Disabled by default..use with caution!)" + "\n" +
    "##-heart" + "\n" +
    "## Enable kernel poll and a few async threads" + "\n" +
    "##+K true" + "\n" +
    "##+A 5" + "\n" +
    "## Increase number of concurrent ports/sockets" + "\n" +
    "##-env ERL_MAX_PORTS 4096" + "\n" +
    "## Tweak GC to run more often" + "\n" +
    "##-env ERL_FULLSWEEP_AFTER 10" + "\n" +
    "# Enable SMP automatically based on availability" + "\n" +
    "-smp auto")
    vmfile.close()

    subprocess.call(["mkdir", "%s" % (Name)])
    subprocess.call(["./erlstore", "console"], cwd="./bin")

def main(input_list):

    if len(input_list) == 1:
        generate_files([input_list[0], "master", "127.0.0.1", "4010"])

    else:
        try:
        
            if len(input_list) != 4:
                raise "Wrong number of arguments!"

        except Exception as e:
            print e
            print >> sys.stderr, "Usage: python2 master.py <name> <Master_IP> <Master_Port>"
        
        generate_files(input_list)

    return 0

if __name__ == '__main__':
    main(sys.argv)
