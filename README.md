
websocket-editor
================

A simple web-based text editor which interacts bidirectionally with
a Haskell backend server via websockets.

The intended use case is live-coding and IDE prototypes. Compared to
a full-featured IDE like VSCode with LSP, this is very lightweight and
allows live content feedback from the server.

The editor component is CodeMirror 5.60 by Marijn Haverbeke: <https://codemirror.net/>.


Communication protocol
----------------------

The communication protocol is a very simple, ad-hoc text-based encoding:

The client (the web-based editor) simply sends editor buffer updates in the format:

    l1:c1-l2:c2,replacement

The server can send the following messages:

    f,feedback_window_text
    e,error_window_text 
    c,new_editor_content
    d,l1:c1-l2:c2,replacement

which has the following meanings:

* `f` replaces the content of the "feedback pane"    
* `e` replaces the content of the "error pane"
* `c` replaces the content of the editor
* `d` applies an atomic update on the editor buffer


LaTeX-style unicode input
-------------------------

Using the `d` command above, we can implement a simple LaTeX-style unicode method:
Simply type `\alpha` and press TAB to finalize (or autocomplete, if there is a unique
completion). 

