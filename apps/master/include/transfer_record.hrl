-record(transfer, {socket, server, port, assoc, file, fd, filesize, timestamp, window = 10}).

%%% Packet Header ENUM declarations
-define(PJOIN,       <<1:8>>).  %% Proxy join request

-define(CURRLIST,    <<2:8>>).  %% Request for list from proxy
-define(SNDLIST,     <<3:8>>).  %% Sending list to proxy

-define(GETFLOC,     <<4:8>>).  %% Proxy request for file
-define(NEWFLOC,     <<5:8>>).  %% Proxy request for new loc
-define(SNDFLOC,     <<6:8>>).  %% Sending loc to proxy

-define(FSDOWN,      <<7:8>>).  %% Proxy (or FS) tells FS down
-define(PQUIT,       <<8:8>>).  %% Proxy asks to quit

%% Fileserver Commands
-define(FSJOIN,      <<9:8>>).  %% FS join request

-define(GETSEC,      <<10:8>>). %% FS ask for second loc
-define(SNDSEC,      <<11:8>>). %% Proxy gives second loc
-define(DLT,         <<12:8>>). %% FS asks delete
-define(UPDTTS,      <<13:8>>). %% FS asks new timestamp

-define(FSQUIT,      <<14:8>>). %% FS asks to quit

%% Backup Commands
-define(GETCHNG,     <<15:8>>). %% Backup asks for changes
-define(SNDCHNG,     <<16:8>>). %% Sending changes

-define(NEWMSTR,     <<17:8>>). %% Global broadcast of new master
-define(ERROR,       <<18:8>>). %% Error response

-define(NEWSECFLOC,  <<19:8>>). %% FS requests a second file location

-define(BJOIN, <<20:8>>).

-define(RRQ,   <<21:8>>).
-define(DATA,  <<22:8>>).
-define(ACK,   <<23:8>>).
-define(SND,   <<25:8>>).
