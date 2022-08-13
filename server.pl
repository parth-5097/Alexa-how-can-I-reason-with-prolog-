:- use_module(library(http/http_server)).

:- use_module(alexa_mod).

% Prolog Webserver Part I

server(Port) :-
  http_server([port(Port)]).

:- http_handler(root(.),
                http_redirect(moved, location_by_id(alexa)),
                []).
:- http_handler(root(alexa), alexa, []).

main :- server(5000).