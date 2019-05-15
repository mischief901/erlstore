%%%----------------------------------------------------------------------------
%%%
%%% Tufts COMP 112 - Spring 2018
%%% Networks Project 1
%%%
%%% Name: Alex Misch
%%% Email: Alexander.Misch@tufts.edu
%%%
%%% Name: Matt Turner
%%% Email: Matthew.Turner@tufts.edu
%%%
%%% transport.erl
%%%
%%%----------------------------------------------------------------------------

-module(transport).
-include("transfer_record.hrl").
-include("inet.hrl").
-include("inet_sctp.hrl").

%% Universal File Transport Functions
-export([to_header/1, from_header/1]).
-export([read_file/1, send_file/1, get_packet_number/1, receive_data/1, 
	 write_data/2, send_delete/1, send_sec/1, get_file_path/1]).

%% Number of bytes of data to send in a packet.
-define(DATA_SIZE,  1024).

%% to_binary( Name, Size ) creates a binary field of the specified length
%%    with the input string Name, padded out with 0's to the specified 
%%    field Size. Internal.
to_binary(Name, _Size) when is_binary(Name) -> Name;
to_binary(Name, _Size) ->
    list_to_binary(Name).

%% to_binary(Name, Size) ->
%%     Name2 = list_to_binary(Name),
%%     Name_size = byte_size(Name2),
%%     Rem_Name = (Size - Name_size) * 8,
%%     list_to_binary([Name2, <<0:Rem_Name>>]).


%% to_header(ENUM, Args) creates a binary packet of the specified size
%%    for the given packet type for UDP transport. Internal.
to_header({?PJOIN}) ->
    list_to_binary([?PJOIN]);

to_header({?CURRLIST}) ->
    list_to_binary([?CURRLIST]);

to_header({?SNDLIST, Full_list}) ->
    Full_list1 = [?SNDLIST | Full_list],
    list_to_binary([Full_list1]);

to_header({?GETFLOC, Filename}) ->
    File_name2 = to_binary(Filename, 100),
    list_to_binary([?GETFLOC, File_name2]);

to_header({?NEWFLOC, Filename, Filesize}) ->
    Filename2 = to_binary(Filename, 100),
    list_to_binary([?NEWFLOC, <<Filesize:64>>, Filename2]);

to_header({?SNDFLOC, Filename, {IP, Port}}) ->
    Filename2 = to_binary(Filename, 100),
    IP1 = from_IP(IP),
    list_to_binary([?SNDFLOC, IP1, <<Port:16>>, Filename2]);

to_header({?FSDOWN, {IP, Port}}) ->
    IP1 = from_IP(IP),
    list_to_binary([?FSDOWN, IP1, <<Port:16>>]);

to_header({?PQUIT, {IP, Port}}) ->
    IP1 = from_IP(IP),
    list_to_binary([?PQUIT, IP1, <<Port:16>>]);

to_header({?FSJOIN}) ->
    list_to_binary([?FSJOIN]);

to_header({?GETSEC}) ->
    list_to_binary([?GETSEC]);

to_header({?SNDSEC, {IP, Port}}) ->
    IP1 = from_IP(IP),
    list_to_binary([?SNDSEC, IP1, <<Port:16>>]);

to_header({?UPDTTS, Filename, {IP, Port}, {Sec_IP, Sec_Port}}) ->
    Filename2 = to_binary(Filename, 100),
    IP1 = from_IP(IP),
    IP2 = from_IP(Sec_IP),
    list_to_binary([?UPDTTS, IP1, <<Port:16>>, IP2, <<Sec_Port:16>>, Filename2]);

to_header({?FSQUIT, {IP, Port}}) ->
    IP1 = from_IP(IP),
    list_to_binary([?FSQUIT, IP1, <<Port:16>>]);

to_header({?GETCHNG}) ->
    list_to_binary([?GETCHNG]);

to_header({?SNDCHNG, Changes}) ->
    Changes1 = [?SNDCHNG | Changes],
    list_to_binary([Changes1]);

to_header({?NEWMSTR, {IP, Port}}) ->
    IP1 = from_IP(IP),
    list_to_binary([?NEWMSTR, IP1, <<Port:16>>]);

to_header({?NEWSECFLOC, Filename, Filesize}) ->
    Filename2 = to_binary(Filename, 100),
    list_to_binary([?NEWSECFLOC, <<Filesize:64>>, Filename2]);

to_header({?BJOIN}) ->
    list_to_binary([?BJOIN]);

to_header({?RRQ, Window, Timestamp, File_Name}) ->
    File_Name2 = to_binary(File_Name, 100),
    list_to_binary([?RRQ, <<Window:1/unit:8>>, <<Timestamp:1/unit:16>>, 
		      File_Name2]);

to_header({?DATA, Sequence, Data}) ->
    Data_Bin = to_binary(Data, ?DATA_SIZE),
    list_to_binary([?DATA, <<Sequence:32>>, Data_Bin]);

to_header({?ACK, Sequence, File_Name})  ->
    File_Name2 = to_binary(File_Name, 100),
    list_to_binary([?ACK, <<Sequence:32>>, File_Name2]);

to_header({?ERROR, File_Name}) ->
    File_Name2 = to_binary(File_Name, 100),
    list_to_binary([?ERROR, File_Name2]);

to_header({?SND, Window, Timestamp, File_Name}) ->
    File_Name2 = to_binary(File_Name, 100),
    list_to_binary([?SND, <<Window:1/unit:8>>, 
		      <<Timestamp:1/unit:16>>, File_Name2]);

to_header({?DLT, File_Name}) ->
    File_Name2 = to_binary(File_Name, 100),
    list_to_binary([?DLT, File_Name2]);

to_header({?SEC, Window, Timestamp, File_Name}) ->
    File_Name2 = to_binary(File_Name, 100),
    list_to_binary([?SEC, <<Window:1/unit:8>>,
		    <<Timestamp:1/unit:16>>, File_Name2]);

to_header(Otherwise) ->
    io:format("to_header error: ~p~n", [Otherwise]).


%% from_header(Message) parses the packet type of an incoming SCTP packet and
%%    casts the packet into its appropriate data fields. Internal.
from_header(Message)->
    <<Type:1/unit:8, Rest/binary>> = Message,
    case Type of
	% PJOIN request
	1 ->
	    {join};
	    	
	% CURRLIST request
	2 ->
	    {currlist};
	
	% SNDLIST message
	3 ->
	    case byte_size(Rest) of
		116 ->
		    <<Filename:100/unit:8, Timestamp:4/unit:8, PIP_Raw:4/unit:8, 
		      PPort:2/unit:8, SIP_Raw:4/unit:8, SPort:2/unit:8>> = Rest,
		    PIP = to_IP(PIP_Raw),
		    SIP = to_IP(SIP_Raw),
		    {sndlist, {binary_to_list(Filename), Timestamp, {PIP, PPort}, {SIP, SPort}}};
		110 ->
		    <<Filename:100/unit:8, Timestamp:4/unit:8, PIP_Raw:4/unit:8, 
		      PPort:2/unit:8>> = Rest,
		    PIP = to_IP(PIP_Raw),
		    {sndlist, {binary_to_list(Filename), Timestamp, {PIP, PPort}, {}}};
		Other ->
		    io:format("Error reading file record: ~p~n", [Other])
			
	    end;	

	% GETFILELOCATION request
	4 ->
	    <<Filename/binary>> = Rest,
            {getfloc, binary_to_list(Filename)};
	    
	% GETNEWFILELOCATION request 
	5 -> 
	    <<Filesize:64, Filename/binary>> = Rest,
	    {newfloc, binary_to_list(Filename), Filesize};

	% SNDFLOC
	6 ->
	    <<IP1:8, IP2:8, IP3:8, IP4:8, Port:16, File_Name/binary>> = Rest,
	    IP = {IP1, IP2, IP3, IP4},
	    io:format("IP:Port ~p:~p~n", [IP, Port]),
	    io:format("File: ~p~n", [File_Name]),
	    {sndfloc, binary_to_list(File_Name), {IP, Port}};
	    
	% FS Down
	7 ->
	    <<IP_raw:4/unit:8, Port:2/unit:8>> = Rest,
	    IP = to_IP(IP_raw),
	    {fsdown, {IP, Port}};
	
	% PQUIT request
	8 ->
	    <<IP_raw:4/unit:8, Port:2/unit:8>> = Rest,
	    IP = to_IP(IP_raw),
	    {pquit, {IP, Port}};
	
	%% FSJOIN request
	9 ->
	    {fsjoin};

	% GETSEC request
	10 ->
	    {getsec};

	% SNDSEC
	11 ->
	    <<IP1:8, IP2:8, IP3:8, IP4:8, Port:2/unit:8, Filename/binary>> = Rest,
	    IP = {IP1, IP2, IP3, IP4},
	    {sndsec, binary_to_list(Filename), {IP, Port}};

	% DLT request
	12 ->
	    <<Filename/binary>> = Rest,
	    {dlt, binary_to_list(Filename)};
	
	% UPDTTS Request
	13 ->
	    <<IP1:8, IP2:8, IP3:8, IP4:8, Port1:2/unit:8, 
	      Sec_IP1:8, Sec_IP2:8, Sec_IP3:8, Sec_IP4:8, 
	      Port2:2/unit:8, Filename/binary>> = Rest,
	    IP = {IP1, IP2, IP3, IP4},
	    Sec_IP = {Sec_IP1, Sec_IP2, Sec_IP3, Sec_IP4},
	    {updtts, binary_to_list(Filename), {IP, Port1}, {Sec_IP, Port2}};

	% FSQUIT request
	14 ->
	    <<IP_raw:4/unit:8, Port:2/unit:8>> = Rest,
	    IP = to_IP(IP_raw),
	    {fsquit, {IP, Port}};

	% GETCHNG request
	15 ->
	    {getchng};

	% SNDCHNG
	16 ->
	    case byte_size(Rest) of
		116 ->
		    <<Filename:100/unit:8, Timestamp:4/unit:8, PIP_Raw:4/unit:8, 
		      PPort:2/unit:8, SIP_Raw:4/unit:8, SPort:2/unit:8>> = Rest,
		    PIP = to_IP(PIP_Raw),
		    SIP = to_IP(SIP_Raw),
		    {sndlist, {binary_to_list(Filename), Timestamp, {PIP, PPort}, {SIP, SPort}}};
		110 ->
		    <<Filename:100/unit:8, Timestamp:4/unit:8, PIP_Raw:4/unit:8, 
		      PPort:2/unit:8>> = Rest,
		    PIP = to_IP(PIP_Raw),
		    {sndlist, {binary_to_list(Filename), Timestamp, {PIP, PPort}, {}}};
		Other ->
		    io:format("Error reading file record: ~p~n", [Other])
			
	    end;	
	    
	% NEWMSTR request
	17 ->
	    <<IP_raw:4/unit:8, Port:2/unit:8>> = Rest,
	    IP = to_IP(IP_raw),
	    {newmstr, {IP, Port}};

	% NEWSECFLOC request
	19 ->
	    <<Filesize:64, Filename/binary>> = Rest,
	    {newsecfloc, binary_to_list(Filename), Filesize};
	20 ->
	    {backup};
	% DATA
	22 -> 
	    <<Sequence:4/unit:8, Data/binary>> = Rest,
	    {data, Sequence, Data};

	% ACK
	23 ->
	    <<Sequence:4/unit:8, File_Name/binary>> = Rest,
	    {ack, Sequence, binary_to_list(File_Name)};

	% SND
	25 ->
	    <<Window:1/unit:8, Timestamp:1/unit:16, File_Name/binary>> = Rest,
	    {snd, Window, Timestamp, binary_to_list(File_Name)};

	% SEC
	30 ->
	    <<Window:1/unit:8, Timestamp:1/unit:16, File_Name/binary>> = Rest,
	    {sec, Window, Timestamp, binary_to_list(File_Name)};

	% ERROR
	28 ->
	    {error, Rest};
	% RRQ
	21 -> 
	    <<Window:1/unit:8, Timestamp:1/unit:16, File_Name/binary>> = Rest,
	    {rrq, Window, Timestamp, binary_to_list(File_Name)};
	% DLT
	26 ->
	    <<File_Name/binary>> = Rest,
	    {dlt, binary_to_list(File_Name)};
	Type ->
	    io:format("Error received type: ~p with ~p~n", [Type, Rest])
    end.

to_IP(IP_raw) ->
    <<IP1:1/unit:8, IP2:1/unit:8, IP3:1/unit:8, IP4:1/unit:8>> = IP_raw,
    {IP1, IP2, IP3, IP4}.

from_IP({IP1, IP2, IP3, IP4}) ->
    <<IP1, IP2, IP3, IP4>>.
    

%% read_file sends a request to 
%%    obtain the file File_Name from the file server Server, listening on port
%%    Server_Port. On success, file transfer commences from Server via SCTP. 
%%    Error on failure. External.
read_file(#transfer{socket = Socket, 
		    assoc = Assoc,
		    file = File,
		    window = Window, 
		    timestamp = Timestamp} = Transfer) ->

    Request = to_header({?RRQ, Window, Timestamp, File}),
    ok = gen_sctp:send(Socket, Assoc, 0, Request),
    io:format("Read Request sent.~n"),
    Response = receive_response(Transfer),

    case Response of
	{0, File} ->
	    receive_data(Transfer);
	Other ->
	    io:format("Other received: ~p~n", [Other])
    end.

receive_response(#transfer{socket = Socket, 
			   server = FromIP, 
			   port = FromPort,
			   file = File,
			   assoc = Assoc} = Transfer) ->


    case gen_sctp:recv(Socket) of
	
	{ok, {_,_,_,{sctp_assoc_change,comm_lost,_,_,_,_}}} ->
	    receive_response(Transfer);

	{error, timeout} ->
	    {error, timeout};

	{error, {_,_,{sctp_send_failed, _,_,_,_,_}}} ->
	    receive_response(Transfer);
	
	{ok, {FromIP, FromPort, _SndRcv, 
	      {sctp_paddr_change,_One,_Two,_Three, Four}}} ->
	    read_file(Transfer#transfer{assoc=Assoc#sctp_assoc_change{assoc_id=Four}});

	{ok, {FromIP, FromPort, _SndRcv, Packet}} ->
	    case from_header(Packet) of
		{ack, 0, File} ->
		    io:format("Ack received: ~p.~n", [0]),
		    {0, File};
		{error, File} ->
		    io:format("error, File: ~p~n", [File]),
		    {error, File};
		{snd, _Window, _Timestamp, Filename} ->
		    {0, Filename};
		Other ->
		    io:format("error, Other: ~p~n", [Other]),
		    receive_response(Transfer)
	    end		
    end.

send_file(#transfer{socket = Socket, 
		    assoc = Assoc,
		    file = File, 
		    window = Window, 
		    timestamp = Timestamp} = Transfer) ->
    
    io:format("Sending file.~n"),
    %% Request to send a file
    SND_Request = to_header({?SND, Window, Timestamp, 
			       File}),
    ok = gen_sctp:send(Socket, Assoc, 0, SND_Request),   
    io:format("Send File packet sent.~n"),

    Response = send_response(Transfer),

    case Response of
	%% Error bookkeeping to take in send - passed to send_file() caller
	{error, timeout} ->
	    send_file(Transfer);
	{error, File} ->
	    {error, File};

	{ok, File} ->
	    {ok, File};

	%% seq. 0 indicated start
	{0, File} ->
	    Res = send_loop(Transfer),

	    case Res of 
		{error, disconnected} ->
		    % statem has to close FD and Socket
		    {error, File};
		{ok, File} ->
		    {ok, File}
		end
	end.

%% send_sec_file transfers the file from a primary location to a secondary.
send_sec(#transfer{socket = Socket, 
		    assoc = Assoc,
		    file = File, 
		    window = Window, 
		    timestamp = Timestamp} = Transfer) ->
    
    io:format("Sending secondary.~n"),
    %% Request to send a file
    SND_Request = to_header({?SEC, Window, Timestamp, 
			       File}),
    io:format("Sending secondary request: ~p~n", [SND_Request]),
    ok = gen_sctp:send(Socket, Assoc, 0, SND_Request),   
    io:format("Secondary File packet sent.~n"),

    Response = send_response(Transfer),

    case Response of
	%% Error bookkeeping to take in send - passed to send_file() caller
	{error, File} ->
	    {error, File};
	%% seq. 0 indicated start
	{0, File} ->
	    Res = send_loop(Transfer),

	    case Res of 
		{error, disconnected} ->
		    % statem has to close FD and Socket
		    {error, File};
		{ok, File} ->
		    {ok, File}
		end
	end.

%% send_response({Socket, Server_Port, File_Name})
%%
%% Listens for a response from a server which indicates it is ready
%%   to receive a file. Internal.
%%
%% Inputs:  Socket      - The socket to listen to
%%          Server_Port - The port to listen on
%%          File_Name   - The name of the file to be sent
%%
%% Output:  A well-formed response from the server.

send_response(#transfer{socket = Socket, 
			server = FromIP, 
			port = FromPort,
			file = File,
			assoc = Assoc} = Transfer) ->

    case gen_sctp:recv(Socket, 5000) of
	
	{ok, {_,_,_,{sctp_assoc_change,comm_lost,_,_,_,_}}} ->
	    send_response(Transfer);

	{error, timeout} ->
	    {error, timeout};

	{error, {_,_,{sctp_send_failed, _,_,_,_,_}}} ->
	    send_response(Transfer);
	
	{ok, {FromIP, FromPort, _SndRcv, 
	      {sctp_paddr_change,_One,_Two,_Three, Four}}} ->
	    send_file(Transfer#transfer{assoc=Assoc#sctp_assoc_change{assoc_id = Four}});

	{ok, {FromIP, FromPort, _SndRcv, Packet}} ->
	    case from_header(Packet) of
		{ack, 0, File} ->
		    io:format("Ack received: ~p.~n", [0]),
		    {0, File};
		{error, File} ->
		    {error, File};
		_Other ->
		    send_response(Transfer)
	    end		
    end.

get_packet_number(File_Size) when File_Size rem ?DATA_SIZE > 0 ->
    (File_Size div ?DATA_SIZE) + 2;

get_packet_number(File_Size) ->
    File_Size div ?DATA_SIZE + 1.

send_loop(#transfer{filesize = File_Size} = Transfer) ->
    
    
    io:format("Filesize is: ~p~n", [File_Size]),
    Packets = get_packet_number(File_Size),
    io:format("Packet number is: ~p~n", [Packets]),
    send_loop(Transfer, Packets, 1).

send_loop(#transfer{window = Window} = Transfer, Packets, Seq) 
	  when Seq + Window < Packets ->
   
    %% send window number of file segments and wait for acks.
    %% on timeout or success, loop on new sequence number.
    
    {ok, Last_Sent} = send_window(Transfer, Seq, Window),
    
    io:format("Last_Sent: ~p~n", [Last_Sent]),

    {ok, Ack} = receive_ack(Transfer, Last_Sent),

    io:format("Received ack. Sending more data.~n"),

    send_loop(Transfer, Packets, Ack);

send_loop(#transfer{socket = Socket,
		     assoc = Assoc} = Transfer, Packets, Seq) ->
    
    %% send last window of packets.
    {ok, Last_Sent} = send_window(Transfer, Seq, Packets - Seq),

    %%io:format("Receiving last acks~n"),
    io:format("Sent: ~p~n", [Last_Sent]),
    io:format("Packets: ~p~n", [Packets]),
    
    case Last_Sent of
	Packets ->
	    Packet = to_header({?DATA, Last_Sent, <<>> }),
	    io:format("Last Packet: ~p~n", [Packet]),
	    gen_sctp:send(Socket, Assoc, 0, Packet),

	    io:format("Waiting for last ack.~n"),
	    last_ack(Transfer, Packets, Seq);
	_Other ->
	    {ok, Next} = receive_ack(Transfer, Last_Sent),
	    send_loop(Transfer, Packets, Next)
    end.

last_ack(#transfer{socket = Socket, file=File,
		     assoc = Assoc} = Transfer, Packets, Seq) ->

    case gen_sctp:recv(Socket) of
	{ok, {_IP, _Port, _, {sctp_assoc_change, _, _, _, _, _}=Assoc2}} ->
	    last_ack(Transfer#transfer{assoc=Assoc2}, Packets, Seq);


	{ok, {_IP, _Port, _, {sctp_paddr_change, _, _, _, Assoc_Id}}} ->
	    last_ack(Transfer#transfer{assoc=
					   Assoc#sctp_assoc_change{assoc_id = Assoc_Id}}, 
		     Packets, Seq);

	{ok, {_IP, _Port, _, {sctp_shutdown_event, _}}} ->
	    %%gen_sctp:eof(Socket, Assoc),
	    {ok, File};

	{ok, {_IP, _Port, _, Data}} when byte_size(Data) =:= 5 ->
	    io:format("Last Ack received: ~p~n", [Data]),
	    %%gen_sctp:eof(Socket, Assoc),
	    {ok, File};

	{ok, {_IP, _Port, _, Data}} ->
	    io:format("Last Ack error: ~p, size: ~p ~n", [Data, byte_size(Data)]),
	    %%gen_sctp:abort(Socket, Assoc),
	    {ok, File}
    end.


send_window(_Transfer, Seq, 0) ->
    {ok, Seq};

send_window(#transfer{socket = Socket,
		       assoc = Assoc} = Transfer, Seq, Sent) ->
    
    io:format("Current queue to send: ~p~n", [Sent]),
    case get_packet(Transfer, Seq) of
	{ok, eof} ->
	    Packet = to_header({?DATA, Seq, <<>> }),
	    io:format("Last Packet: ~p~n", [Packet]),
	    gen_sctp:send(Socket, Assoc, 0, Packet),
	    {ok, Seq};
	{ok, Data} ->
	    Packet = to_header({?DATA, Seq, Data}),
	    gen_sctp:send(Socket, Assoc, 0, Packet),
	    send_window(Transfer, Seq+1, Sent-1);
	{error, Reason} ->
	    io:format("Error reading from file: ~p~n", [Reason]),
	    {error, Reason}
    end.

get_packet(#transfer{fd = FD, filesize = Size} = _Transfer, Seq) 
  when Size < (Seq * ?DATA_SIZE) ->

    case file:pread(FD, (Seq - 1) * ?DATA_SIZE, Size rem ?DATA_SIZE) of
	{ok, Data} -> 
	    {ok, Data};
	eof -> 
	    io:format("EOF reached~n"),
	    {ok, eof};
	{error, Reason} -> 
	    io:format("Error reading from file: ~p~n", [Reason]),
	    {error, Reason}
    end;

get_packet(#transfer{fd=FD}, Seq) ->
    
    case file:pread(FD, (Seq - 1) * ?DATA_SIZE, ?DATA_SIZE) of
	{ok, Data} -> 
	    {ok, Data};
	eof -> 
	    io:format("EOF reached~n"),
	    {ok, eof};
	{error, Reason} -> 
	    {error, Reason} 
    end.

receive_ack(#transfer{socket = Socket, assoc=Assoc,
		      file = File} = Transfer, Last_Sent) ->
    
    io:format("waiting to receive ack of window.~n"),
    case gen_sctp:recv(Socket) of
	
	{ok, {_IP, _Port, _Anc, {sctp_assoc_change, _,_,_,_,_} = New_Assoc}} ->  
     	    receive_ack(Transfer#transfer{assoc = New_Assoc}, Last_Sent);

	{ok, {_IP, _Port, _Anc, {sctp_paddr_change, _,_,_,Assoc_id}}} ->
     	    receive_ack(Transfer#transfer{assoc = 
					      Assoc#sctp_assoc_change{assoc_id=Assoc_id}},
			Last_Sent);

	{ok, {_IP, _Port, _Anc, Packet}} when 
	      byte_size(Packet) =:= 0 ->
	    {ok, Last_Sent};
	
	{ok, {_IP, _Port, _Anc, Packet}} ->
	    {ack, Seq, File} = from_header(Packet),
	    {ok, Seq}
    end.

receive_data(#transfer{socket = Socket,
		       assoc = Assoc, window = Window,
		       file = File} = Transfer) ->
    Packet = to_header({?ACK, 0, File}),

    gen_sctp:send(Socket, Assoc, 0, Packet),
    receive_data(Transfer, 1, Window,#{}).

receive_data(#transfer{socket = Socket, 
		       assoc = Assoc,
		       file = File,
		       window = Window} = Transfer, 
	     Received, 0, Acc) ->
    
    Packet = to_header({?ACK, Received, File}),

    gen_sctp:send(Socket, Assoc, 0, Packet),
    io:format("Window Ack Packet Sent: ~p~n", [Packet]),
    receive_data(Transfer, Received, Window, Acc);

receive_data(#transfer{socket = Socket, 
		       assoc = Assoc} = Transfer, 
	     Received, Remaining, Acc) ->

    case gen_sctp:recv(Socket) of
	{ok, {_IP, _Port, _AncData, {sctp_paddr_change, _,_,_,Assoc_id}}} ->
     	    receive_data(Transfer#transfer{assoc= 
					       Assoc#sctp_assoc_change{assoc_id=Assoc_id}},
			 Received, Remaining, Acc);

	{ok, {_IP, _Port, _, {sctp_assoc_change, _, _, _, _, _}=Assoc2}} ->
	    receive_data(Transfer#transfer{assoc=Assoc2}, 
			 Received, Remaining, Acc);

	{ok, {_IP, _Port, _Assoc, Packet}} ->
	
		    case from_header(Packet) of 
			{data, Seq, Data} when byte_size(Data) > 0 ->
			    io:format("Data Packet ~B received~n", [Seq]),
			    receive_data(Transfer, Received+1, Remaining - 1, 
					 maps:put(Seq,Data,Acc));
			
			{data, Seq, Data} when byte_size(Data) =:= 0 ->
			    io:format("Last Data Packet received~n"),
			    io:format("Packet Num: ~p~n", [Seq]),
			    Packet1 = to_header({?ACK, Seq, ""}),
			    gen_sctp:send(Socket, Assoc, 0, Packet1),
			    maps:put(Seq, Data, Acc);
						

			{sctp_paddr_change, _,_,_,Assoc_Id} ->
			    receive_data(Transfer#transfer{
					   assoc = Assoc#sctp_assoc_change{
						     assoc_id=Assoc_Id}},
					 Received, Remaining, Acc);
			Other ->
			    io:format("Error received: ~p~n", [Other])
		    end;

	Other ->
	    io:format("Error receiving data: ~p~n", [Other])
    end.

write_data(FD, Data) ->
    io:format("Writing data~n"),
    List = maps:to_list(Data),
    Sorted = lists:keysort(1, List),
    {_Keys, Values} = lists:unzip(Sorted),
    file:write(FD, list_to_binary(Values)).
    

send_delete(#transfer{socket = Socket,
		      assoc = Assoc,
		      file = File} = _Transfer) -> 
    
    DLT_Request = to_header({?DLT, File}),

    ok = gen_sctp:send(Socket, Assoc, 0, DLT_Request).

get_file_path(File_Name) ->
    Node = atom_to_list(node()),
    Folder = string:split(Node, "@", leading),
    Path = filename:join([hd(Folder), File_Name]),
    Path.









