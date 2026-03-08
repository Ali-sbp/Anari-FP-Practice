module MyEvolModule where 

--import Data.List(sort)

data MyEvolution = 
    LUCA 
    | Cyanobacteria 
    | Trilobite 
    | Ichthyostega
    | Dimetrodon
    | Archaeopteryx
    | Morganucodon
    | Purgatorius
    | Australopithecine
    | Humans

instance Show MyEvolution where 
    show LUCA = "Last Universal Common Ancestor"
    show Cyanobacteria = "Synechococcus"
    show Trilobite ="Paradoxides"
    show Ichthyostega = "Ichthyostega"
    show Dimetrodon = "Dimetrodon"
    show Archaeopteryx = "Archaeopteryx"
    show Morganucodon = "Morganucodon"
    show Purgatorius = "Purgatorius"
    show Australopithecine = "Australopithecus Afarensis"
    show Humans = "Homo Sapiens"

instance Eq MyEvolution where 
    LUCA == LUCA = True 
    Cyanobacteria == Cyanobacteria = True 
    Trilobite == Trilobite = True 
    Ichthyostega == Ichthyostega = True 
    Dimetrodon == Dimetrodon = True 
    Archaeopteryx == Archaeopteryx = True 
    Morganucodon == Morganucodon = True 
    Purgatorius == Purgatorius = True
    Australopithecine == Australopithecine = True
    Humans == Humans = True
    _ == _ = False 

instance Ord MyEvolution where 
    compare LUCA LUCA = EQ
    compare LUCA _ = LT
    compare _ LUCA = GT
    compare Cyanobacteria Cyanobacteria = EQ
    compare Cyanobacteria _ = LT
    compare _ Cyanobacteria = GT
    compare Trilobite Trilobite = EQ
    compare Trilobite _ = LT
    compare _ Trilobite = GT
    compare Ichthyostega Ichthyostega = EQ
    compare Ichthyostega _ = LT
    compare _ Ichthyostega = GT
    compare Dimetrodon Dimetrodon = EQ
    compare Dimetrodon _ = LT
    compare _ Dimetrodon = GT
    compare Archaeopteryx Archaeopteryx = EQ
    compare Archaeopteryx _ = LT
    compare _ Archaeopteryx = GT
    compare Morganucodon Morganucodon = EQ
    compare Morganucodon _ = LT
    compare _ Morganucodon = GT
    compare Purgatorius Purgatorius = EQ
    compare Purgatorius _ = LT
    compare _ Purgatorius = GT
    compare Australopithecine Australopithecine = EQ
    compare Australopithecine _ = LT
    compare _ Australopithecine = GT
    compare Humans Humans = EQ

instance Enum MyEvolution where 
    fromEnum LUCA                = 0
    fromEnum Cyanobacteria       = 1
    fromEnum Trilobite           = 2
    fromEnum Ichthyostega        = 3
    fromEnum Dimetrodon          = 4
    fromEnum Archaeopteryx       = 5
    fromEnum Morganucodon        = 6
    fromEnum Purgatorius         = 7
    fromEnum Australopithecine   = 8
    fromEnum Humans              = 9
    toEnum 0 = LUCA
    toEnum 1 = Cyanobacteria
    toEnum 2 = Trilobite
    toEnum 3 = Ichthyostega
    toEnum 4 = Dimetrodon
    toEnum 5 = Archaeopteryx
    toEnum 6 = Morganucodon
    toEnum 7 = Purgatorius
    toEnum 8 = Australopithecine
    toEnum 9 = Humans
    toEnum _ = error "MyEvolution.toEnum: out of range"
instance Bounded MyEvolution where 
    minBound = LUCA
    maxBound = Humans

instance Read MyEvolution where 
    readsPrec _ str = case str of
        'L':'a':'s':'t':' ':'U':'n':'i':'v':'e':'r':'s':'a':'l':' ':'C':'o':'m':'m':'o':'n':' ':'A':'n':'c':'e':'s':'t':'o':'r':rest -> [(LUCA, rest)]
        'S':'y':'n':'e':'c':'h':'o':'c':'o':'c':'c':'u':'s':rest                                                                     -> [(Cyanobacteria, rest)]
        'P':'a':'r':'a':'d':'o':'x':'i':'d':'e':'s':rest                                                                             -> [(Trilobite, rest)]
        'I':'c':'h':'t':'h':'y':'o':'s':'t':'e':'g':'a':rest                                                                         -> [(Ichthyostega, rest)]
        'D':'i':'m':'e':'t':'r':'o':'d':'o':'n':rest                                                                                 -> [(Dimetrodon, rest)]
        'A':'r':'c':'h':'a':'e':'o':'p':'t':'e':'r':'y':'x':rest                                                                     -> [(Archaeopteryx, rest)]
        'M':'o':'r':'g':'a':'n':'u':'c':'o':'d':'o':'n':rest                                                                         -> [(Morganucodon, rest)]
        'P':'u':'r':'g':'a':'t':'o':'r':'i':'u':'s':rest                                                                             -> [(Purgatorius, rest)]
        'A':'u':'s':'t':'r':'a':'l':'o':'p':'i':'t':'h':'e':'c':'u':'s':' ':'A':'f':'a':'r':'e':'n':'s':'i':'s':rest                 -> [(Australopithecine, rest)]
        'H':'o':'m':'o':' ':'S':'a':'p':'i':'e':'n':'s':rest                                                                         -> [(Humans, rest)]
        _ -> []

data MyEvolution' = LUCA'
                  | Cyanobacteria'
                  | Trilobite'
                  | Ichthyostega'
                  | Dimetrodon'
                  | Archaeopteryx'
                  | Morganucodon'
                  | Purgatorius'
                  | Australopithecine'
                  | Humans'
                  deriving (Show, Read, Eq, Ord, Enum, Bounded)

