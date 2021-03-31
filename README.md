
websocket-editor
================

A simple web-based text editor which interacts bidirectionally with
a Haskell backend server via websockets.

The intended use case is live-coding and simple IDE prototypes. Compared to
a full-featured IDE like VSCode with LSP, this is very lightweight and
allows live content feedback from the server.

The editor component is CodeMirror 5.60 by Marijn Haverbeke: <https://codemirror.net/>.


Multiple buffers
----------------

The server has the responsibility to maintain the content of the buffers.
There is a simple tab-based interface to select buffers.


LaTeX-style unicode input
-------------------------

Using the `d` command below, we can implement a simple LaTeX-style unicode method:
Simply type `\alpha` and press TAB to finalize (or autocomplete, if there is a unique
completion). 


Communication protocol
----------------------

The communication protocol is a very simple, ad-hoc text-based encoding:

The client (the web-based editor) can send the following message:

    u,l1:c1-l2:c2,replace    # the buffer was updated
    b,<idx>                  # change the active buffer (indexing from zero)
    l                        # list the existing buffers

The server can send the following messages:

    f,text                   # replaces the "feedback pane"          
    e,text                   # replaces the "error pane"      
    c,text                   # replaces the editor content      
    d,l1:c1-l2:c2,replace    # applies an atomic update on the editor buffer      
    v,50                     # sets the vertical split ratio 
    h,75                     # sets the horizontal split ratio 
    l,["name1","name2"]      # returns a list of buffers

which has the following meanings:

* `f` replaces the content of the "feedback pane"    
* `e` replaces the content of the "error pane"
* `c` replaces the content of the editor
* `d` applies an atomic update on the editor buffer
* `v` sets the vertical split ratio (percentage, the size of the left pane)
* `h` sets the horizontal split ratio (percentage, the size of the top pane)


