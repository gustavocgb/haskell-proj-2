Setup inicial
-------------


> {-# OPTIONS_GHC -Wall #-}

> module Main where

> import ParseLib
> import Test.Tasty
> import Test.Tasty.HUnit
  
> main :: IO ()
> main = defaultMain tests

> tests :: TestTree
> tests
>   = testGroup "Unit tests"
>         [
>            question01aTests
>         ,  question01bTests
>         ,  question02aTests
>         ,  question02bTests
>         ,  question03aTests
>         ,  question03bTests
>         ]

> parse :: Parser s a -> [s] -> Maybe a
> parse p s = if null res then Nothing
>             else Just $ fst (head res)
>      where
>        res = runParser p s

Turtle-oriented programming
===========================

Introdução
-----------

Nesta avaliação, você deverá implementar um conjunto de funções para realizar o parsing de
uma pequena linguagem de programação para movimentar uma tartaruga em um plano 2D.

Desenvolvimento do parser
-------------------------

Questão 1. Nessa questão desenvolveremos um conjunto de funções para
realizar o parsing de uma posição da tartaruga no plano. Posições são
representadas pelo seguinte tipo:

> data Position
>       = Position {
>            x :: Int
>         ,  y :: Int
>         } deriving (Eq, Ord, Show)

que representa as coordenadas da tartaruga no plano cartesiano.

a) Desenvolva o parser:

> -- Usei a propria documentação da biblioteca de apoio
> -- link: https://hackage.haskell.org/package/uu-tc-2015.1.1/docs/ParseLib-Simple-Derived.html
> -- (*>) :: Applicative f => f a -> f b -> f b
> -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
> -- (<$>) :: Functor f => (a -> b) -> f a -> f b

> -- parseNumber :: Parser Char Int
> -- parseNumber = undefined

> -- Explicação: como o objetivo é ignorar espaços em branco, na lib tem o TokenParsing 'whitespace'. E para extrair somente os números naturais, na lib tem o
> -- TokenParsing 'natural'. 

> parseNumber :: Parser Char Int
> parseNumber =  whitespace *> natural


que processa números inteiros descartando espaços antes e depois da
string processada. Sua implementação deve satisfazer os seguintes
casos de teste.

> question01aTests :: TestTree
> question01aTests
>       = testGroup "Question 01-a Tests"
>                    [
>                       testCase "Question 01-a success" $
>                           parse parseNumber "11" @?= Just 11
>                    ,  testCase "Question 01-a success spaces right" $
>                           parse parseNumber "11 " @?= Just 11
>                    ,  testCase "Question 01-a success spaces left" $
>                           parse parseNumber " 11" @?= Just 11
>                    ,  testCase "Question 01-a success spaces both" $
>                           parse parseNumber "  11  " @?= Just 11
>                    ,  testCase "Question 01-a failure no number" $
>                           parse parseNumber "abc" @?= Nothing
>                    ,  testCase "Question 01-a failure empty" $
>                           parse parseNumber "" @?= Nothing
>                    -- ,  testCase "Question 01-a failure many numbers" $
>                    --       parse parseNumber " 70 89 76  " @?= Just 70
>                    ]

b) Desenvolva a função

> -- parsePosition :: Parser Char Position
> -- parsePosition = undefined

> -- Explicação: Como o tipo Position tem a coordenada x e y. É necessário fazer o fmap para cada coordenada com a função parseNumber
> -- mencionada anteriormente

> parsePosition :: Parser Char Position
> parsePosition = Position <$> parseNumber <*> parseNumber

que realiza o processamento de uma posição da tartaruga no plano.
Uma posição é representada por um par de números naturais separados
por um ou mais espaços em branco. Sua implementação deve satisfazer
os seguintes casos de teste.

> question01bTests :: TestTree
> question01bTests
>       = testGroup "Question 01-b Tests"
>                    [
>                       testCase "Question 01-b success" $
>                           parse parsePosition "11 22" @?= Just (Position 11 22)
>                    ,  testCase "Question 01-b success spaces right" $
>                           parse parsePosition "11 22  " @?= Just (Position 11 22)
>                    ,  testCase "Question 01-b success spaces left" $
>                           parse parsePosition " 11 22" @?= Just (Position 11 22)
>                    ,  testCase "Question 01-b success spaces both" $
>                           parse parsePosition "  11 22 " @?= Just (Position 11 22)
>                    ,  testCase "Question 01-b failure no number" $
>                           parse parsePosition "abc" @?= Nothing
>                    ,  testCase "Question 01-b failure empty" $
>                           parse parsePosition "" @?= Nothing
>                    ]

Questão 2. Nesta questão desenvolveremos um conjunto de funções para
implementar um parsing do estado inicial da tartaruga no plano que é
representado pelo seguinte tipo.

> data Turtle
>       = Turtle {
>           position :: Position
>         , facing   :: Facing
>         } deriving (Eq, Ord, Show)

O tipo `Facing` denota qual a direção em que a tartaruga está caminhando no plano.

> data Facing = North | South | East | West deriving (Eq, Ord, Show)

2-a) Desenvolva a função

> -- parseFacing :: Parser Char Facing
> -- parseFacing = undefined

> -- Explicação: Nessa questão usei a mesma ideia da função parseNumber para tirar os espaçoes em branco, porém nessa o retorno é uma string.
> -- E para caracterizar o tipo e identificar o Char usei o identifier. Eu não estou conseguindo fazer a captura da string como um array para
> -- manipular corretamente e não consegui realizar o otherwise, com isso esta dando warning.

> parseString :: Parser Char String  
> parseString = whitespace *> identifier

> parseFacing :: Parser Char Facing
> parseFacing = f <$> parseString
>       where         
>         f x
>           | x == "N" = North
>           | x == "S" = South
>           | x == "W" = West
>           | x == "E" = East


que realiza o parsing de uma representação da direção da tartaruga. Representaremos
a posição `North` pelo caractere `N`, `South` por `S`, `East` por `E` e `West` por
`W`. Sua função deve satisfazer os seguintes casos de teste:

> question02aTests :: TestTree
> question02aTests
>       = testGroup "Question 02-a Tests"
>                    [
>                       testCase "Question 02-a success" $
>                           parse parseFacing "N" @?= Just North
>                    ,  testCase "Question 02-a success spaces right" $
>                           parse parseFacing "N  " @?= Just North
>                    ,  testCase "Question 02-a success spaces left" $
>                           parse parseFacing " N" @?= Just North
>                    ,  testCase "Question 02-a success spaces both" $
>                           parse parseFacing "  N " @?= Just North
>                    ,  testCase "Question 02-a failure invalid char" $
>                           parse parseFacing "1bc" @?= Nothing
>                    ,  testCase "Question 02-a failure empty" $
>                           parse parseFacing "" @?= Nothing
>                    --  ,  testCase "Question 02-a success" $
>                    --         parse parseFacing "S" @?= Just South
>                    ]

2-b) Desenvolva a função

> --parseTurtle :: Parser Char Turtle
> --parseTurtle = undefined 

> -- Explicação: Como o tipo Turtle tem Postion e Facing. Utilizei as funções anteriores referente aos atributos do tipo parar realizar parsing

> parseTurtle :: Parser Char Turtle
> parseTurtle = Turtle <$> parsePosition <*> parseFacing

que processa o estado inicial de uma tartaruga no plano de execução da linguagem
turtle. Sua função deve atender os seguintes casos de teste:

> tur :: Maybe Turtle
> tur = Just (Turtle (Position 11 22) North)

> question02bTests :: TestTree
> question02bTests
>       = testGroup "Question 02-b Tests"
>                    [
>                       testCase "Question 02-b success" $
>                           parse parseTurtle "11 22 N" @?= tur
>                    ,  testCase "Question 02-b success spaces right" $
>                           parse parseTurtle "11 22 N  " @?= tur
>                    ,  testCase "Question 02-b success spaces left" $
>                           parse parseTurtle " 11 22 N" @?= tur
>                    ,  testCase "Question 02-b success spaces both" $
>                           parse parseTurtle "  11 22 N " @?= tur
>                    ,  testCase "Question 02-b failure invalid char" $
>                           parse parseTurtle "a 11 bc" @?= Nothing
>                    ,  testCase "Question 02-b failure empty" $
>                           parse parseTurtle "" @?= Nothing
>                    ]

Questão 3. O objetivo desta questão é o desenvolvimento de um conjunto de funções para fazer o parsing
de programas da linguagem turtle.

3-a) Instruções da linguagem turtle são representadas pelo seguinte tipo de dados:

> data Instr
>    = Forward | ToLeft | ToRight | Print
>      deriving (Eq, Ord, Show)

Desenvolva a função

> -- parseInstr :: Parser Char Instr
> -- parseInstr = undefined

> -- Explicação: Nessa questão usei a mesma ideia da função parseFacing, porém nessa para caracterizar Instr.

> parseInstr :: Parser Char Instr
> parseInstr = f <$> parseString
>       where         
>         f x
>           | x == "F" = Forward
>           | x == "L" = ToLeft
>           | x == "R" = ToRight
>           | x == "P" = Print



que realiza o parsing de uma instrução turtle. Cada instrução é representada por uma letra, como se
segue: `Forward` é representada pela letra `F`, `ToLeft` por `L`, `ToRight` por `R` e `Print` por `P`.
Seu parser deve satisfazer os seguintes casos de teste.

> question03aTests :: TestTree
> question03aTests
>       = testGroup "Question 03-a Tests"
>                    [
>                       testCase "Question 03-a success" $
>                           parse parseInstr "F" @?= Just Forward
>                    ,  testCase "Question 03-a success spaces right" $
>                           parse parseInstr "F" @?= Just Forward
>                    ,  testCase "Question 03-a success spaces left" $
>                           parse parseInstr " F" @?= Just Forward
>                    ,  testCase "Question 03-a success spaces both" $
>                           parse parseInstr "  F " @?= Just Forward
>                    ,  testCase "Question 03-a failure invalid char" $
>                           parse parseInstr "a 11 bc" @?= Nothing
>                    ,  testCase "Question 03-a failure empty" $
>                           parse parseInstr "" @?= Nothing
>                    ]

3-b) Programas completos da linguagem turtle são expressos pela configuração inicial
da tartaruga no plano e lista de instruções a serem executadas. O tipo `Program`
representa programas turtle:

> data Program
>       = Program {
>           start :: Turtle
>         , code  :: [Instr]
>         } deriving (Eq, Ord, Show)

O campo `start` representa a posição inicial e code a lista de instruções que deve ser executada.
Com base no apresentado, implemente a função:

> parseProgram :: Parser Char Program
> parseProgram = undefined 

> -- Explicação: Não conseguir terminar essa questão. Mas, a estrategia que pretendia usar é
> -- usar a função parseTurtle para o start e fazer uma função auxiliar onde eu conseguisse iterar
> -- com a função parseInstr pra retornar um array de Instr

> --parseCode :: Parser Char [Instr]
> --parseCode = parseInstr

> --parseProgram :: Parser Char Program
> --parseProgram = Program <$> parseTurtle <*> parseCode 

para realizar o parsing de programas turtle. Seu parser deve atender os seguintes casos de teste.

> prog :: Program
> prog = Program (Turtle (Position 11 22) North)
>                [Forward, ToLeft, Forward, Forward]

> question03bTests :: TestTree
> question03bTests
>       = testGroup "Question 03-b Tests"
>                    [
>                       testCase "Question 03-b success" $
>                           parse parseProgram "11 22 N FLFF" @?= Just prog
>                    ,  testCase "Question 03-b success spaces right" $
>                           parse parseProgram "11 22 N FLFF  " @?= Just prog
>                    ,  testCase "Question 03-b success spaces left" $
>                           parse parseProgram " 11 22 N FLFF" @?= Just prog
>                    ,  testCase "Question 03-b success spaces both" $
>                           parse parseProgram "  11 22 N FLFF  " @?= Just prog
>                    ,  testCase "Question 03-b failure invalid char" $
>                           parse parseProgram "b 11 bc" @?= Nothing
>                    ,  testCase "Question 03-b failure empty" $
>                           parse parseProgram "" @?= Nothing
>                    ]

