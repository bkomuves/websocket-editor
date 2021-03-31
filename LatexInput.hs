
-- | LaTeX-style input mode for unicode symbols

module LatexInput 
  ( latexSymbolTrie
  , latexLookup
  , latexLookupUnique
  , latexCompletion
  ) where

--------------------------------------------------------------------------------

import Prelude hiding ( succ , pi , div )

import Data.Char

-- import qualified Data.Map as Map 
-- import Data.Map (Map)

import qualified Trie as Trie 
import Trie (Trie)

--------------------------------------------------------------------------------

{-
-- * for testing

greeks = 
  [ alpha, beta, gamma, delta, epsilon, zeta, eta, theta
  , iota, kappa, lambda, mu, nu, xi                        
  -- , omicron  
  , pi_, rho, sigma, tau, upsilon, phi, chi, psi, omega 
  ]

math_test =
  [ forall_ , alpha, beta, gamma , '.' , ' ' 
  , alpha , ostar , '(' , beta , oplus , gamma , ')' , ' ' , '=' 
  , alpha , ostar, beta , oplus , alpha, ostar , gamma
  ]
  
-}

--------------------------------------------------------------------------------

{-
latexSymbolMap :: Map String Char
latexSymbolMap = Map.fromList latexSymbolList

latexLookupMap :: String -> Maybe Char
latexLookupMap s = Map.lookup s latexSymbolTable
-}

latexSymbolTrie :: Trie Char
latexSymbolTrie = Trie.fromList latexSymbolList

latexLookup :: String -> Maybe Char
latexLookup s = Trie.lookup s latexSymbolTrie

latexCompletion :: String -> Maybe Char
latexCompletion s = Trie.lookupCompletion_ s latexSymbolTrie

latexLookupUnique :: String -> Maybe Char
latexLookupUnique s = Trie.lookupUnique s latexSymbolTrie

--------------------------------------------------------------------------------

abc    = ['A'..'Z'] ++ ['a'..'z'] :: [Char]
abc123 = abc ++ ['0'..'9']        :: [Char]

latexSymbolList :: [(String,Char)]
latexSymbolList 
  =  latexSymbolList1
  ++ [ ("bb"  ++ [ch] , mathbb      ch) | ch <- abc123 ]    -- blackboard
  ++ [ ("cal" ++ [ch] , mathcal     ch) | ch <- abc    ]    -- calligraphic 
  ++ [ ("bsc" ++ [ch] , mathbscript ch) | ch <- abc    ]    -- bold script (different font in STIX than calligraphic!)
  ++ [ ("fr"  ++ [ch] , mathfrak    ch) | ch <- abc    ]    -- fraktur
  ++ [ ("bfr" ++ [ch] , mathbfrak   ch) | ch <- abc    ]    -- bold fraktur
  ++ [ ("sf"  ++ [ch] , mathsf      ch) | ch <- abc123 ]    -- sans serif
  ++ [ ("bsf" ++ [ch] , mathbsf     ch) | ch <- abc123 ]    -- bold sans serif
  ++ [ ("tt"  ++ [ch] , mathtt      ch) | ch <- abc123 ]    -- teletype
  ++ [ ("ea"  ++ [ch] , enclosedAlphaNum  ch) | ch <- abc123 ]    -- enclosed alphanumericals

--------------------------------------------------------------------------------

latexSymbolList1 :: [(String,Char)]
latexSymbolList1 = 
  [ 
    "forall"  ~> forall_
  , "exist"   ~> exists
  , "nexist"  ~> nexists
  , "complement" ~> complement 
  , "emptyset"   ~> emptyset
  , "partial"    ~> partial
    --
  , "in"         ~> element      
  , "notin"      ~> notelement 
  , "elem"       ~> element
  , "nelem"      ~> notelement
  , "notelem"    ~> notelement
  , "ni"         ~> rev_element  
  , "notni"      ~> rev_notelement 
  , "infty"      ~> infinity
  , "infinity"   ~> infinity
    --
  , "leq"  ~> leq
  , "lneq" ~> lneq
  , "nleq" ~> nleq
  , "geq"  ~> geq
  , "gneq" ~> gneq
  , "ngeq" ~> ngeq
    --
  , "/="  ~> neq
  , "=="  ~> equiv
  , "/==" ~> nequiv
  , "~"   ~> sim
  , "/~"   ~> nsim
  , "~~"   ~> approx
  , "/~~"  ~> napprox
  , "neq" ~> neq
  , "equiv"  ~> equiv
  , "nequiv" ~> nequiv
  , "sim"    ~> sim
  , "nsim"   ~> nsim
  , "simeq"  ~> simeq
  , "nsimeq" ~> nsimeq
  , "cong"    ~> cong
  , "ncong"   ~> ncong
  , "congneq" ~> congneq
  , "approx"  ~> approx
  , "napprox" ~> napprox
  , "asymp"  ~> asymp
  , "nasymp" ~> nasymp

  , "=def" ~> '\x225d'
  , "=m"   ~> '\x225e'
  , "=?"   ~> '\x225f'
  , "=."   ~> '\x2250'
  , "=o"   ~> '\x2257'
  , "=star"  ~> '\x225b'
  , "=ast"   ~> '\x2a6e'
  , "=*"     ~> '\x2a6e'
  , "=tri"   ~> '\x225c'
  , "=frown" ~> '\x2258'
  , "=wedge" ~> '\x2259'
  , "=vee"   ~> '\x225a'

  , "::"   ~> '\x2237'
  , ":="   ~> '\x2254'
  , "=:"   ~> '\x2255'
  , "::="  ~> '\x2a74'

    --
  , "_"       ~> openbox
  , "dagger"  ~> dagger
  , "ddagger" ~> ddagger
  , "lfloor"  ~> lfloor
  , "rfloor"  ~> rfloor
  , "lceil"   ~> lceil
  , "rceil"   ~> rceil

  , "?" ~> '\x00bf'    -- upside down ?
  , "!" ~> '\x00a1'    -- upside down !
  , "degree" ~> degree

  , "pm" ~> pm
  , "mp" ~> mp
  , "o"      ~> circ
  , "circ"   ~> circ
  , "bullet" ~> bullet
  , "wedge" ~> wedge
  , "vee"   ~> vee
  , "cap" ~> cap
  , "cup" ~> cup
  , "uplus" ~> uplus
  , "udot"  ~> udot
  , "u+"  ~> uplus
  , "u."  ~> udot
  , "sqcap" ~> sqcap
  , "sqcup" ~> sqcup
  , "glb"   ~> sqcap
  , "lub"   ~> sqcup

  , "|-" ~> vdash
  , "-|" ~> dashv
  , "T"   ~> top
--  , "_|_" ~> bot       -- we want `\_` (open box) to be fast to type
  , "vdash" ~> vdash
  , "dashv" ~> dashv
  , "top"   ~> top
  , "bot"   ~> bot
  , "nvdash" ~> nvdash
  , "|="      ~> models
  , "models"  ~> models
  , "nmodels" ~> nmodels
  
  , "pipe"   ~> '|'          -- single pipe
  , "dpipe"  ~> '\x2016'     -- double pipe - it matches in size/design the normal pipe character
  
  , "|"        ~> '\x2223'     -- divides (larger than pipe!)
  , "div"      ~> '\x2223'     -- divides
  , "/|"       ~> '\x2224'     -- does not divide
  , "ndiv"     ~> '\x2224'     -- does not divide

  , "||"  ~> '\x2225'     -- "||"
  , "par" ~> '\x2225'     -- parallel
  , "/||"  ~> '\x2226'     -- "||"
  , "npar" ~> '\x2226'     -- parallel

    -- triangles (like normal subgroup)
  , "ltri" ~> ltri       
  , "rtri" ~> rtri
  , "ltrieq" ~> ltrieq
  , "rtrieq" ~> rtrieq
  , "nltri"   ~> nltri       
  , "nrtri"   ~> nrtri
  , "nltrieq" ~> nltrieq
  , "nrtrieq" ~> nrtrieq
  , "lhd" ~> ltri       
  , "rhd" ~> rtri
  , "unlhd" ~> ltrieq   -- latex shorthands, doesn't make much sense to me
  , "unrhd" ~> rtrieq

    --
  , "prod"    ~> prod
  , "coprod"  ~> coprod
  , "sum"     ~> sum_
  , "int"     ~> integral
  , "amalg"   ~> amalg

    -- operators
  , "neg"    ~> neg
  , "x"      ~> times     -- it's ok, we have to press TAB after, so you can still type \x -> ...
  , "times"  ~> times
  , "%"        ~> divsign
  , "divsign"  ~> divsign
  , "diamond" ~> diamond
  , "."       ~> cdot
  , "cdot"    ~> cdot
  , "*"       ~> asterix
  , "ast"     ~> asterix
  , "star"    ~> star

  , "wstar"   ~> whiteStar   
  , "bstar"   ~> blackStar 
  , "cstar"   ~> encircledWhiteStar

  , "bowtie"  ~> bowtie
  , "lbowtie"  ~> lbowtie
  , "rbowtie"  ~> rbowtie

  , "oplus"  ~> oplus
  , "ominus" ~> ominus
  , "otimes" ~> otimes
  , "oslash"  ~> oslash
  , "odiv"    ~> odiv
  , "obslash" ~> obslash
  , "odot"   ~> odot
  , "ostar"  ~> ostar
  , "ocirc"  ~> ocirc
  , "oast"   ~> oast
  , "oeq"    ~> oeq
  , "obar"   ~> ovminus
  , "opar"   ~> oveq
  , "ovminus" ~> ovminus
  , "oveq"    ~> oveq
  , "olt"    ~> olt
  , "ogt"    ~> ogt
  , "oempty" ~> oempty

  , "o+"  ~> oplus
  , "o-" ~> ominus
  , "ox" ~> otimes
  , "o/"  ~> oslash
  , "ob/" ~> obslash
  , "o%"  ~> odiv
  , "o."   ~> odot
  , "o*"   ~> oast
  , "o="    ~> oeq
  , "o|"    ~> ovminus
  , "o||"   ~> oveq
  , "o<"   ~> olt
  , "o>"   ~> ogt
  , "oo"   ~> ocirc

  , "boxplus"  ~> boxplus
  , "boxminus" ~> boxminus
  , "boxtimes" ~> boxtimes
  , "boxdot"   ~> boxdot
  , "boxcirc"   ~> boxcirc
  , "boxslash"  ~> boxslash
  , "boxbslash" ~> boxbslash
  , "boxast"    ~> boxast
  , "boxbox"    ~> boxbox
  , "boxempty"  ~> boxempty

  , "box+"  ~> boxplus
  , "box-" ~> boxminus
  , "boxx" ~> boxtimes
  , "box."  ~> boxdot
  , "boxo"  ~> boxcirc
  , "box/"  ~> boxslash
  , "boxb/" ~> boxbslash
  , "box*"  ~> boxast

    -- greek
  , "alpha"   ~> alpha
  , "beta"    ~> beta
  , "gamma"   ~> gamma
  , "delta"   ~> delta
  , "epsilon" ~> epsilon
  , "zeta"    ~> zeta
  , "eta"     ~> eta
  , "theta"   ~> theta
  , "iota"    ~> iota   
  , "kappa"   ~> kappa  
  , "lambda"  ~> lambda  
  , "mu"      ~> mu   
  , "nu"      ~> nu   
  , "xi"      ~> xi   
  , "pi"      ~> pi_
  , "rho"     ~> rho    
  , "sigma"   ~> sigma
  , "tau"     ~> tau
  , "upsilon" ~> upsilon 
  , "phi"     ~> phi
  , "chi"     ~> chi
  , "psi"     ~> psi 
  , "omega"   ~> omega  
    -- capital greek
  , "Gamma"  ~> capitalGamma
  , "Delta"  ~> capitalDelta
  , "Theta"  ~> capitalTheta 
  , "Lambda" ~> capitalLambda
  , "Xi"     ~> capitalXi
  , "Pi"     ~> capitalPi
  , "Sigma"  ~> capitalSigma
  , "Phi"    ~> capitalPhi
  , "Psi"    ~> capitalPsi
  , "Omega"  ~> capitalOmega
  , "Upsilon"  ~> capitalUpsilon

    -- std blackboard numbers
  , "A" ~> aa
  , "P" ~> pp
  , "Z" ~> zz
  , "N" ~> nn
  , "Q" ~> qq
  , "R" ~> rr
  , "C" ~> cc

    -- some letterlike
  , "ell"   ~> ell
  , "hbar"  ~> hbar
  , "wp"    ~> wp
  , "Re"    ~> re
  , "Im"    ~> im
  , "nabla"   ~> nabla
  , "laplace" ~> laplace

  , "i"     ~> dotless_i
  , "j"     ~> dotless_j

  , "aleph" ~> aleph 
  , "beth"  ~> beth
  , "gimel" ~> gimel 
  , "dalet" ~> dalet

  , "b" ~> flat
  , "#" ~> sharp
  , "flat"    ~> flat
  , "neutral" ~> neutral
  , "sharp"   ~> sharp

  , "maltese"  ~> maltese
  , "scissors" ~> scissors
  , "phone"    ~> phone

  , "quarter"  ~> '\x2669'    -- 1/4 note
  , "eighth"   ~> '\x266a'    -- 1/8 note
  , "eighth2"  ~> '\x266b'    -- beamed 1/8 notes
  
  , "cent"  ~> cent
  , "pound" ~> pound
  , "yen"   ~> yen
  , "euro"  ~> euro

  , "copy"  ~> copyright
  , "(c)"   ~> copyright
  , "(C)"   ~> copyright
  , "(r)"   ~> registered
  , "(R)"   ~> registered
  , "(p)"   ~> recording
  , "(P)"   ~> recording
  , "tm"    ~> trademark
  , "TM"    ~> trademark

  , "S"       ~> section
  , "section" ~> section
  , "para"    ~> pilcrow    -- ?
  , "pilcrow" ~> pilcrow

    -- brackets
  , "<" ~> langle
  , ">" ~> rangle
  , "langle" ~> langle
  , "rangle" ~> rangle
  , "[[" ~> lwsqb
  , "]]" ~> rwsqb
  , "{|" ~> '\x2983'
  , "|}" ~> '\x2984'
  , "((" ~> '\x2985'
  , "))" ~> '\x2986'
  , "(|" ~> '\x2987'
  , "|)" ~> '\x2988'
  , "<|" ~> '\x2989'
  , "|>" ~> '\x298a'
  , "[|" ~> '\x27ec'    -- tortoise shell bracket
  , "|]" ~> '\x27ed'    -- tortoise shell bracket

  , ".<<" ~> lguill
  , ".>>" ~> rguill
  , ".<"  ~> lsingleguill
  , ".>"  ~> rsingleguill

    -- astrological signs
  , "aries"   ~> aries
  , "taurus"  ~> taurus
  , "gemini"  ~> gemini
  , "cancer"  ~> cancer
  , "leo"     ~> leo
  , "virgo"   ~> virgo
  , "libra"   ~> libra
  , "scorpio" ~> scorpio
  , "saggit"  ~> saggitarius
  , "capri"   ~> capricorn
  , "aqua"    ~> aquarius
  , "pisces"  ~> pisces
    -- planets
  , "earth"   ~> earth
  , "mercury" ~> mercury
  , "jupiter" ~> jupiter
  , "saturn"   ~> saturn
  , "uranus"   ~> uranus    -- not present ??
  , "neptun"   ~> neptun
  , "pluto"    ~> pluto

    -- arrows
  , "leftarrow"  ~> leftarrow
  , "uparrow"    ~> uparrow
  , "rightarrow" ~> rightarrow
  , "downarrow"  ~> downtarrow
  , "lrarrow"    ~> leftrightarrow
  , "udarrow"    ~> updownarrow
  , "nwarrow" ~> nwarrow
  , "nearrow" ~> nearrow 
  , "searrow" ~> searrow 
  , "swarrow" ~> swarrow
     -- double arrows 
  , "dleftarrow"  ~> dleftarrow
  , "duparrow"    ~> duparrow
  , "drightarrow" ~> drightarrow
  , "ddownarrow"  ~> ddowntarrow
  , "dlrarrow"    ~> dleftrightarrow
  , "dudarrow"    ~> dupdownarrow
  , "dnwarrow" ~> dnwarrow
  , "dnearrow" ~> dnearrow 
  , "dsearrow" ~> dsearrow 
  , "dswarrow" ~> dswarrow 
    -- more arrows
  , "<-"      ~> leftarrow
  , "->"      ~> rightarrow
  , "<->"     ~> leftrightarrow
  , "<="      ~> dleftarrow
  , "=>"      ~> drightarrow
  , "<=>"     ~> dleftrightarrow
  , "-o"      ~> lollipop
  , "o-"      ~> leftlollipop
  , "o-o"      ~> bilollipop
  , "mapsto"   ~> mapsto
  , "lollipop" ~> lollipop
  , "llollipop" ~> leftlollipop
  , "bilollipop" ~> bilollipop

    -- inequalities
  , "/<" ~> nlt
  , "/>" ~> ngt
  , "nlt" ~> nlt
  , "ngt" ~> ngt
  , "ll" ~> ll
  , "gg" ~> gg
  , "lll" ~> lll
  , "ggg" ~> ggg
  , "leq"  ~> leq
  , "geq"  ~> geq
  , "nleq"  ~> nleq
  , "ngeq"  ~> ngeq
  , "lneq"  ~> lneq
  , "gneq"  ~> gneq
    --
  , "prec" ~> prec
  , "succ" ~> succ
  , "nprec" ~> nprec
  , "nsucc" ~> nsucc
  , "preceq" ~> preceq
  , "succeq" ~> succeq
  , "npreceq" ~> npreceq
  , "nsucceq" ~> nsucceq
   
    -- subset
  , "subset"    ~> subset
  , "supset"    ~> supset
  , "nsubset"   ~> nsubset
  , "nsupset"   ~> nsupset
  , "subseteq"  ~> subseteq
  , "supseteq"  ~> supseteq
  , "nsubseteq" ~> nsubseteq
  , "nsupseteq" ~> nsupseteq
  , "subsetneq" ~> subsetneq
  , "supsetneq" ~> supsetneq
     --
  , "sqsubset"   ~> sqsubset
  , "sqsupset"   ~> sqsupset
  , "sqsubseteq" ~> sqsubseteq
  , "sqsupseteq" ~> sqsupseteq
  , "nsqsubseteq" ~> nsqsubseteq
  , "nsqsupseteq" ~> nsqsupseteq
  , "sqsubsetneq" ~> sqsubsetneq
  , "sqsupsetneq" ~> sqsupsetneq

    -- misc
  , "female" ~> female
  , "male"   ~> male
  , "dice1" ~> dice1
  , "dice2" ~> dice2
  , "dice3" ~> dice3
  , "dice4" ~> dice4
  , "dice5" ~> dice5
  , "dice6" ~> dice6
  , "sad"   ~> sad
  , "happy" ~> happy
  , "diamonds" ~> diamonds
  , "clubs"    ~> clubs
  , "hearts"   ~> hearts
  , "spades"   ~> spades
  , "wdiamonds" ~> wdiamonds
  , "wclubs"    ~> wclubs
  , "whearts"   ~> whearts
  , "wspades"   ~> wspades

    -- dingbat enclosed numbers
  , "b1"  ~> '\x2780'
  , "b2"  ~> '\x2781'
  , "b3"  ~> '\x2782'
  , "b4"  ~> '\x2783'
  , "b5"  ~> '\x2784'
  , "b6"  ~> '\x2785'
  , "b7"  ~> '\x2786'
  , "b8"  ~> '\x2787'
  , "b9"  ~> '\x2788'
  , "b10" ~> '\x2789'

  , "w1"  ~> '\x278a'
  , "w2"  ~> '\x278b'
  , "w3"  ~> '\x278c'
  , "w4"  ~> '\x278d'
  , "w5"  ~> '\x278e'
  , "w6"  ~> '\x278f'
  , "w7"  ~> '\x2790'
  , "w8"  ~> '\x2791'
  , "w9"  ~> '\x2792'
  , "w10" ~> '\x2793'

  ]
  where
    (~>) a b = (a,b)

--------------------------------------------------------------------------------
-- * misc other symbols

openbox = '\x2423'   -- blank 

diamonds = '\x2666'
hearts   = '\x2665'
clubs    = '\x2663'
spades   = '\x2660'

wdiamonds = '\x2662'
whearts   = '\x2661'
wclubs    = '\x2667'
wspades   = '\x2664'

sad   = '\x2639'
happy = '\x263a'

dice1 = '\x2680'
dice2 = '\x2681'
dice3 = '\x2682'
dice4 = '\x2683'
dice5 = '\x2684'
dice6 = '\x2685'

earth   = '\x2641'
mercury = '\x263f'
jupiter = '\x2643'
saturn  = '\x2644'
uranus  = '\x2645'    -- not present in STIX2 ?
neptun  = '\x2646'
pluto   = '\x2647'

female  = '\x2640'
male    = '\x2642'

aries  = '\x2648' 
taurus = '\x2649'
gemini = '\x264a'    -- not present in STIX2 ?
cancer = '\x264b'
leo    = '\x264c'
virgo  = '\x264d'
libra       = '\x264e'
scorpio     = '\x264f'
saggitarius = '\x2650'
capricorn   = '\x2651'
aquarius    = '\x2652'
pisces      = '\x2653'

--------------------------------------------------------------------------------
-- * brackets

lguill = '\x00ab'
rguill = '\x00bb'
lsingleguill = '\x2039'
rsingleguill = '\x203a'

langle = '\x2329'
rangle = '\x232a'

lceil  = '\x2308'
rceil  = '\x2309'
lfloor = '\x230a'
rfloor = '\x230b'

lwsqb = '\x27e6'     -- "mathematical left white square bracket [["
rwsqb = '\x27e7'     -- "mathematical right white square bracket ]]"

--------------------------------------------------------------------------------
-- * arrows

leftarrow  = '\x2190'
uparrow    = '\x2191'
rightarrow = '\x2192'
downtarrow = '\x2193'
leftrightarrow = '\x2194'
updownarrow    = '\x2195'

dleftarrow  = '\x21d0'  -- double arrows
duparrow    = '\x21d1'
drightarrow = '\x21d2'
ddowntarrow = '\x21d3'
dleftrightarrow = '\x21d4'
dupdownarrow    = '\x21d5'
 
nwarrow = '\x2196'
nearrow = '\x2197'
searrow = '\x2198'
swarrow = '\x2199'

dnwarrow = '\x21d6'
dnearrow = '\x21d7'
dsearrow = '\x21d8'
dswarrow = '\x21d9'

mapsto   = '\x21a6'

lollipop     = '\x22b8'
leftlollipop = '\x27dc'
bilollipop   = '\x29df'

--------------------------------------------------------------------------------
-- * ligatures

ff  = '\xfb00'
fi  = '\xfb01'
fl  = '\xfb02'
ffi = '\xfb03'
ffl = '\xfb04'

--------------------------------------------------------------------------------
-- * some math operators

forall_  = '\x2200'
exists   = '\x2203'
nexists  = '\x2204'

complement = '\x2201'
partial    = '\x2202'
emptyset   = '\x2205'

element    = '\x2208'     
notelement = '\x2209'     
rev_element    = '\x220b'     
rev_notelement = '\x220c'     

infinity = '\x221e'

circ   = '\x2218'
bullet = '\x2219'

degree = '\x00b0'

pm = '\x00b1'
mp = '\x2213'

dagger  = '\x2020'
ddagger = '\x2021'

wedge = '\x2227'
vee   = '\x2228'
cap   = '\x2229'
cup   = '\x222a'
uplus = '\x228e'
udot  = '\x228d'
sqcap = '\x2293'
sqcup = '\x2294'

vdash = '\x22a2'
nvdash = '\x22ac'
dashv = '\x22a3'
top   = '\x22a4'
bot   = '\x22a5'

models  = '\x22a8'
nmodels = '\x22ad'

ltri = '\x22b2'       -- left pointing triangle
rtri = '\x22b3'
ltrieq = '\x22b4'
rtrieq = '\x22b5'
nltri = '\x22ea'
nrtri = '\x22eb'
nltrieq = '\x22ec'
nrtrieq = '\x22ed'

prod     = '\x220f'
coprod   = '\x2210'
sum_     = '\x2211'

amalg = '\x2a3f'     -- upside down pi ~= small coproduct

integral  = '\x222b'
integral2 = '\x222c'
integral3 = '\x222d'

neg     = '\x00ac'
divsign = '\x00f7'
times   = '\x00d7'

oplus  = '\x2295'
ominus = '\x2296'
otimes = '\x2297'
oslash = '\x2298'
odot   = '\x2299'
oast   = '\x229b'
ocirc  = '\x229a'
oeq    = '\x229c'
ostar  = '\x235f'      -- misc. technical symbols
ovminus   = '\x29b6'   -- misc. math symbols B
oveq      = '\x29b7'
obslash   = '\x29b8'
olt       = '\x29c0'
ogt       = '\x29c1'
odiv      = '\x29bc'
oempty    = '\x25cb'

boxplus  = '\x229e'
boxminus = '\x229f'
boxtimes = '\x22a0'
boxdot   = '\x22a1'
boxslash  = '\x29c4'  -- misc. math symbols B
boxbslash = '\x29c5'
boxast   = '\x29c6'
boxcirc  = '\x29c7'
boxbox   = '\x29c8'
boxempty = '\x25a1'

asterix = '\x2217'
diamond = '\x22c4'
cdot    = '\x22c5'
star    = '\x22c6'     -- mathematical star

blackStar = '\x2605'
whiteStar = '\x2606'
encircledWhiteStar = '\x272a'

bowtie  = '\x22c8'
lbowtie = '\x22c9'
rbowtie = '\x22ca'

flat    = '\x266d'
neutral = '\x266e'
sharp   = '\x266f'

scissors = '\x2702'
maltese  = '\x2720'
phone    = '\x260e'

--------------------------------------------------------------------------------
-- * inequality

ll = '\x226a'
gg = '\x226b'

lll = '\x22d8'
ggg = '\x22d9'

nlt = '\x226e'
ngt = '\x226f'

leq  = '\x2264'
geq  = '\x2265'
nleq = '\x2270'
ngeq = '\x2271'

lneq  = '\x2268'     -- ??!?!
gneq  = '\x2269'

prec = '\x227a'
succ = '\x227b'
preceq = '\x227c'
succeq = '\x227d'
nprec = '\x2280'
nsucc = '\x2281'
npreceq = '\x22e0'
nsucceq = '\x22e1'

--------------------------------------------------------------------------------
-- * equality, isomorphism

neq = '\x2260'

equiv  = '\x2261'
nequiv = '\x2262'

sim  = '\x223c'
nsim = '\x2241'

simeq  = '\x2243'
nsimeq = '\x2244'

cong    = '\x2245'
congneq = '\x2246'
ncong   = '\x2247'

approx  = '\x2248'
napprox = '\x2249'

asymp  = '\x224d'
nasymp = '\x226d'

--------------------------------------------------------------------------------
-- * subsets

subset   = '\x2282'
supset   = '\x2283'
nsubset   = '\x2284'
nsupset   = '\x2285'
subseteq = '\x2286'
supseteq = '\x2287'
nsubseteq = '\x2288'
nsupseteq = '\x2289'
subsetneq = '\x228a'
supsetneq = '\x228b'

sqsubset   = '\x228f'
sqsubseteq = '\x2290'
sqsupset   = '\x2291'
sqsupseteq = '\x2292'

nsqsubseteq = '\x22e2'
nsqsupseteq = '\x22e3'
sqsubsetneq = '\x22e4'
sqsupsetneq = '\x22e5'

--------------------------------------------------------------------------------
-- * some letterlike

hbar  = '\x210f'
ell   = '\x2113'
wp    = '\x2118'         -- weierstrass p

aleph = '\x2135'
beth  = '\x2136'
gimel = '\x2137'
dalet = '\x2138'

im = '\x2111'
re = '\x211c' 

nabla   = '\x2207'
laplace = '\x2206'

dotless_i = '\x0131'
dotless_j = '\x0237'

aa = '\x1d538'
pp = '\x2119'
cc = '\x2102'
hh = '\x210d'
nn = '\x2115'
zz = '\x2124'
qq = '\x211a'
rr = '\x211d'

cent  = '\x00a2'
pound = '\x00a3'
yen   = '\x00a5'
euro  = '\x20ac'

section    = '\x00a7'
pilcrow    = '\x00b6'
copyright  = '\x00a9'
recording  = '\x2117'
registered = '\x00ae'
trademark  = '\x2122'

--------------------------------------------------------------------------------
-- * some cyrillic

lje   = '\x0409'
de    = '\x0414'
zhe   = '\x0416'
sha   = '\x0428' 
shcha = '\x0429' 

--------------------------------------------------------------------------------
-- * greek lowercase

alpha   = '\x03b1'
beta    = '\x03b2'
gamma   = '\x03b3'
delta   = '\x03b4'
epsilon = '\x03b5'
zeta    = '\x03b6'
eta     = '\x03b7'
theta   = '\x03b8'
iota    = '\x03b9'
kappa   = '\x03ba'
lambda  = '\x03bb'
mu      = '\x03bc'
nu      = '\x03bd'
xi      = '\x03be'
omicron = '\x03bf'
pi_     = '\x03c0'
rho     = '\x03c1'
sigma   = '\x03c3'
tau     = '\x03c4'
upsilon = '\x03c5'
phi     = '\x03c6'
chi     = '\x03c7'
psi     = '\x03c8'
omega   = '\x03c9'

capitalGamma  = '\x0393'
capitalDelta  = '\x0394'
capitalTheta  = '\x0398'
capitalLambda = '\x039b'
capitalXi     = '\x039e'
capitalPi     = '\x03a0'
capitalSigma  = '\x03a3'
capitalPhi    = '\x03a6'
capitalPsi    = '\x03a8'
capitalOmega  = '\x03a9'
capitalUpsilon = '\x03d2'

--------------------------------------------------------------------------------
-- * math alphabets

-- | blackboard
mathbb :: Char -> Char
mathbb c
  | c == 'C'              = cc
  | c == 'H'              = hh
  | c == 'N'              = nn
  | c == 'P'              = pp
  | c == 'Q'              = qq
  | c == 'R'              = rr
  | c == 'Z'              = zz
  | c >= 'A' && c <= 'Z'  = chr $ o - 64 + 0x1d537
  | c >= 'a' && c <= 'z'  = chr $ o - 96 + 0x1d551
  | c >= '0' && c <= '9'  = chr $ o - 48 + 0x1d7d8
  | otherwise             = error "mathbb: invalid character"
  where
    o = ord c

--------------------

frak_C = '\x212d'
frak_H = '\x210c'
frak_I = '\x2111'
frak_R = '\x211c'
frak_Z = '\x2128'

mathfrak :: Char -> Char
mathfrak c
  | c == 'C'              = frak_C
  | c == 'H'              = frak_H
  | c == 'I'              = frak_I
  | c == 'R'              = frak_R
  | c == 'Z'              = frak_Z
  | c >= 'A' && c <= 'Z'  = chr $ o - 64 + 0x1d503
  | c >= 'a' && c <= 'z'  = chr $ o - 96 + 0x1d51d
  | otherwise             = error "mathfrak: invalid character"
  where
    o = ord c

--------------------

cal_B = '\x212c'
cal_E = '\x2130'
cal_F = '\x2131'
cal_H = '\x210b'
cal_I = '\x2110'
cal_L = '\x2112'
cal_M = '\x2133'
cal_R = '\x211b'
cal_e = '\x212f'
cal_g = '\x210a'
cal_o = '\x2134'

mathcal :: Char -> Char
mathcal c
  | c == 'B'              = cal_B
  | c == 'E'              = cal_E
  | c == 'F'              = cal_F
  | c == 'H'              = cal_H
  | c == 'I'              = cal_I
  | c == 'L'              = cal_L
  | c == 'M'              = cal_M
  | c == 'R'              = cal_R
  | c == 'e'              = cal_e
  | c == 'g'              = cal_g
  | c == 'o'              = cal_o
  | c >= 'A' && c <= 'Z'  = chr $ o - 64 + 0x1d49b
  | c >= 'a' && c <= 'z'  = chr $ o - 96 + 0x1d4b5
  | otherwise             = error "mathcal: invalid character"
  where
    o = ord c

--------------------

mathbscript :: Char -> Char
mathbscript c
  | c >= 'A' && c <= 'Z'  = chr $ o - 65 + 0x1d4d0
  | c >= 'a' && c <= 'z'  = chr $ o - 97 + 0x1d4ea
  | otherwise             = error "mathbscript: invalid character"
  where
    o = ord c

mathbfrak :: Char -> Char
mathbfrak c
  | c >= 'A' && c <= 'Z'  = chr $ o - 65 + 0x1d56c
  | c >= 'a' && c <= 'z'  = chr $ o - 97 + 0x1d586
  | otherwise             = error "mathbfrak: invalid character"
  where
    o = ord c

mathtt :: Char -> Char
mathtt c
  | c >= 'A' && c <= 'Z'  = chr $ o - 65 + 0x1d670
  | c >= 'a' && c <= 'z'  = chr $ o - 97 + 0x1d68a
  | c >= '0' && c <= '9'  = chr $ o - 48 + 0x1d7f6
  | otherwise             = error "mathtt: invalid character"
  where
    o = ord c

mathsf :: Char -> Char
mathsf c
  | c >= 'A' && c <= 'Z'  = chr $ o - 65 + 0x1d5a0
  | c >= 'a' && c <= 'z'  = chr $ o - 97 + 0x1d5ba
  | c >= '0' && c <= '9'  = chr $ o - 48 + 0x1d7e2
  | otherwise             = error "mathsf: invalid character"
  where
    o = ord c

mathbsf :: Char -> Char
mathbsf c
  | c >= 'A' && c <= 'Z'  = chr $ o - 65 + 0x1d5d4
  | c >= 'a' && c <= 'z'  = chr $ o - 97 + 0x1d5ee
  | c >= '0' && c <= '9'  = chr $ o - 48 + 0x1d7ec
  | otherwise             = error "mathbsf: invalid character"
  where
    o = ord c

-- \"Enclosed alphanumerics\"
enclosedAlphaNum :: Char -> Char
enclosedAlphaNum c
  | c >= 'A' && c <= 'Z'  = chr $ o - 65 + 0x24b6
  | c >= 'a' && c <= 'z'  = chr $ o - 97 + 0x24d0
  | c == '0'              = '\x24ea'
  | c >= '1' && c <= '9'  = chr $ o - 49 + 0x2460
  | otherwise             = error "enclosedAlphaNum: invalid character"
  where
    o = ord c

enclosedBlackNum :: Int -> Char
enclosedBlackNum n
  | n>=1 && n<=10 = chr (n - 1 + 0x2780)
  | otherwise     = error "enclosedBlackNum: expecting a number between 1 and 10"

enclosedWhiteNum :: Int -> Char
enclosedWhiteNum n
  | n>=1 && n<=10 = chr (n - 1 + 0x278a)
  | otherwise     = error "enclosedWhiteNum: expecting a number between 1 and 10"

--------------------------------------------------------------------------------
