module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

import Test.HUnit
import Data.Maybe

--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.


-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right
        
        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where 
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines

--Ejercicio 1
procVacio :: Procesador a b
procVacio = const []

procId :: Procesador a a
procId = \x -> [x]

procCola :: Procesador [a] a
procCola = \x -> if null x then [] else tail x

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose = \(Rose _ hijos) -> hijos 

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT at = case at of
                        Nil -> []
                        (Tern _ a b c) -> [a, b, c]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie = \(TrieNodo x _) -> [x]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries  = \(TrieNodo _ x) -> x


--Ejercicio 2

foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT cTern cNil at = case at of
                            Nil -> cNil
                            Tern raiz izq cen der -> cTern raiz (f izq) (f cen) (f der)
                        where f = foldAT cTern cNil

foldRose :: (a -> [b] -> b) -> RoseTree a -> b 
foldRose cRose (Rose rose hijos) = cRose rose (map rec hijos)
                                where rec = foldRose cRose

foldTrie cTrie (TrieNodo maybe hijos) = cTrie maybe (map rec hijos)
                                where rec = (\(char, trie) -> (char, foldTrie cTrie trie))


--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = \lista -> foldr f [] lista
    where f = \x rec -> [x] : rec

sufijos :: Procesador [a] [a]
sufijos = \lista -> foldl (\ac x -> (x : (head ac)) : ac) [[]] (reverse lista)


--Ejercicio 4

-- at = Tern 16 (Tern 1 Nil Nil Nil) (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil)) (Tern 10 Nil Nil Nil)

preorder :: Procesador (AT a) a
preorder = foldAT (\raiz izq cen der -> [raiz] ++ izq ++ cen ++ der) []

inorder :: Procesador (AT a) a
inorder = foldAT (\raiz izq cen der -> izq ++ cen ++ [raiz] ++ der) []

postorder :: Procesador (AT a) a
postorder = foldAT (\raiz izq cen der -> izq ++ cen ++ der ++ [raiz]) []

--Ejercicio 5

-- rt = Rose 1 [Rose 2 [], Rose 3 [Rose 6 [], Rose 7 []], Rose 4 [], Rose 5 []] 

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\rose rec -> rose : concat rec)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\rose rec -> if null rec then [rose] else concat rec)

ramasRose :: Procesador (RoseTree a) [a]
ramasRose =  foldRose (\rose rec -> if null rec then [[rose]] else map (rose :) (concat rec))


--Ejercicio 6
caminos :: Procesador (Trie a) [Char]
caminos = foldTrie (\_ rec -> if null rec then [[]] else [] : concat ((map (\(char, rec2) -> (map (char : ) rec2))) rec))


--Ejercicio 7

palabras :: Procesador (Trie a) [Char]
palabras = foldTrie (\maybe rec -> if null rec then if (isNothing maybe) then [] else [[]] else if (isNothing maybe) then (concat (camino rec)) else [] : (concat (camino rec))) 
                              where camino = (map (\(char, rec2) -> (map (char : ) rec2)))


--Ejercicio 8
-- 8.a)
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc ifcond iftrue iffalse = (\x -> if ifcond x then iftrue x else iffalse x)

esNil :: AT a -> Bool
esNil Nil = True
esNil _ = False


-- 8.b)
-- at = Tern 16 (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil)) (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil)) (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil))

(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) pri seg = (\x -> pri x ++ seg x)

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) pri seg = (\x -> concat (map pri (seg x)))

--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.


{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]

-- Formato de tests: {expresion que espero recibir al llamar a mi funcion} ~=? {llamado a la funcion con el argumento correspondiente}
-- separados por comas, adentro de la lista 'test'

-- Preguntar como hacer funcar las cosas con listas vacias
testsEj1 = test [ -- Casos de test para el ejercicio 1
  -- procVacio
    -- [] ~=? procVacio []
    --[] ~=? procVacio (Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)),
    --[] ~=? procVacio (Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []]),
    --[] ~=? procVacio (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]),
  
  -- procId
    [[1,2,3]] ~=? procId [1,2,3],
    [Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)] ~=? procId (Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)),                       
    [Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []]] ~=? procId (Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []]),
    [TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]] ~=? procId (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]),
  
  --procCola 
    [2,3] ~=? procCola [1,2,3]
    --[] ~=? procCola []

  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  (0,0)       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  -- Unoxuno
    [[3], [1], [4], [1], [5], [9]] ~=? unoxuno  [3,1,4,1,5,9],
    [] ~=? unoxuno "",
    [[1]] ~=? unoxuno [1],
    [["Plp"]] ~=? unoxuno ["Plp"],

    -- Sufijos
    ["Plp", "lp", "p", ""] ~=? sufijos "Plp",
    [""] ~=? sufijos "",
    [[1,2,3], [2,3], [3], []] ~=? sufijos [1,2,3],
    [[True, False, True], [False, True], [True], []] ~=? sufijos [True, False, True]
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  ""       -- Caso de test 1 - expresión a testear
    ~=? ""                             -- Caso de test 1 - resultado esperado
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  0       -- Caso de test 1 - expresión a testear
    ~=? 0                                       -- Caso de test 1 - resultado esperado
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  False       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]

testsEj8a = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8b = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8c = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]