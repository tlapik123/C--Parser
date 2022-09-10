module Parser where

import           Control.Exception              ( SomeException
                                                , catch
                                                , evaluate
                                                )
import           Data.Char                      ( isDigit
                                                , isSpace
                                                )
import           Prelude                 hiding ( minimum )
import           System.IO                      ( hSetEncoding
                                                , stdout
                                                , utf8
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )

-- NEPŘIDÁVEJTE ŽÁDNÉ DALŠÍ IMPORTY!

-- | Tato hodnota vyvolá runtime error, který je rozpoznán v unit testech!
-- Používáme ji pro zatím neimplementované funkce.
-- Je to trochu hack, který by šel vyřešit i elegantněji, ale to jsme ještě nedělali...
fixme :: a
fixme = error "NOT IMPLEMENTED YET"

-----------------------------------------------------------------------------------
--                                  ZADÁNÍ                                        |
-----------------------------------------------------------------------------------
--
-- Naimplementujte zadané funkce v Haskellu.
-- U každé části je popis toho co máte dělat.
-- U každé funkce kterou máte implementovat je TODO, FIXME a testy.
--
-- Testy můžete spustit v GHCi zavoláním funkce 'main'.
-- Testy pro část 1 můžete spustit zavoláním funkce 'testPart1', atd.
--
-- Nezapomeňte na rozumné formátování a idiomatický kód
-- (Style Guide najdete v zadání čtvrtého úkolu).
-- Nesmíte nic importovat.
--
-- Deadline je do konce zkouškového období, tj. do konce června.
--
-- Dohromady můžete získat až 21 bodů.
--
-- Tento parser smíte využívat ve svém zápočtovém programu ;)
--
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
--                               OPAKOVÁNÍ                                        |
-----------------------------------------------------------------------------------
newtype State s a = State {runState :: s -> (s, a)}
-- Připomínám: runState :: State s a -> s -> (s, a)

-- Jako na minulém cvičení:
instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
    fmap f stavovyVypocet = State $ \s ->     -- Nový stavový výpočet bere stav
        let (s', a) = runState stavovyVypocet s -- spustí starý výpočet, čímž dostane nový stav a ačko
        in  (s', f a)                           -- a vrátí nový stav a 'f a :: b'

instance Monad (State s) where
  -- return :: a -> State s a
    return a = State $ \s -> (s, a)

    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    stavovyVypocet >>= f = State $ \s ->
        let (s', a)         = runState stavovyVypocet s -- spustíme starý výpočet, dostaneme nový stav a ačko
            stavovyVypocet' = f a                       -- aplikací f na a dostaneme nový stavový výpočet
        in  runState stavovyVypocet' s'                 -- který spustíme aplikací na nový stav

-- Mezikrok mezi 'Functor' a 'Monad', pro nás irelevantní (ale Haskell je jinak nešťastný).
-- Tahle implementace funguje vždy, pokud je daný typ monáda.
instance Applicative (State s) where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return $ f x

{-| Vrátí aktuální stav

>>> runState get 0
(0,0)
-}
get :: State s s
get = State $ \s -> (s, s)

{-| Nastaví nový stav

>>> runState (set 42) 0
(42,())
-}
set :: s -> State s ()
set s = State $ \_ -> (s, ())

-----------------------------------------------------------------------------------
--                            1. ČÁST - Definice                     [ 5 bodů ]   |
-----------------------------------------------------------------------------------

-- | Chyba při parsování typicky obsahuje dvě věci:
-- * co parser očekával, tj. políčko 'errorExpected'
-- * co parser dostal/nalezl, tj. políčko 'errorFound'
data ParseError = ParseError
    { errorExpected :: String
    , errorFound    :: String
    }
    deriving Eq

-- | Nastavíme 'ParseError'u nějaké rozumné vypisování :)
instance Show ParseError where
    show err =
        "expected: " <> errorExpected err <> ", but found: " <> errorFound err

{- Co by mohl být parser?

* První aproximace je 'String -> a',
ale to nemodeluje situaci, kdy parser selže.

* Parser, který může selhat vypadá jako 'String -> Either ParseError a'.
Tohle ale pořád ještě nestačí, protože nevíme, kolik toho parser spotřeboval! 
Navíc ideálně chceme, aby byl Parser _sekvenční_ a aby šel hezky skládat.

* Dostáváme tedy: 'String -> (String, Either ParseError a)'
což je to samé jako 'State String (Either ParseError a)'
-}
newtype Parser a = Parser {runParser :: State String (Either ParseError a)}
-- runParser má typ 'Parser a -> State String (Either ParseError a)'
-- tj. vezme Parser a vyrobí z něj stavový výpočet

-- | Pomocná funkce, která spustí parser a vrátí to, co naparsoval
-- Neenforcuje EOF -- používejte raději 'run' definované níže, tahle funkce je spíš pro testy...
execParser :: Parser a -> String -> Either ParseError a
execParser p = snd . runState (runParser p)

-- | Pomocná funkce, která spustí parser a vrátí zbylý string, který nenaparsoval.
-- Tahle funkce je hlavně pro testy.
finalStateParser :: Parser a -> String -> String
finalStateParser p = fst . runState (runParser p)

-- Jednoduchý parser, který načte jestli je nalezen end-of-file:
-- 1) Zeptá se stavu na vstup.
-- 2) Pokud je vstup prázdný, uspěje a vrátí hodnotu '()'
-- 3) Pokud je vstup neprázdný, vypíše error
parseEof :: Parser ()
parseEof = Parser $ do
    input <- get
    case input of
        []      -> return $ Right ()
        (c : _) -> return $ Left $ ParseError "end of file" [c]

{- Příklady:

>>> runState (runParser parseEof) ""
("",Right ())

>>> runState (runParser parseEof) "ahoj"
("ahoj",Left expected: end of file, but found: a)
-}

-- TODO
-- Tento parser načte _nějaké_ písmenko, je-li to možné:
-- 1) Zeptá se stavu na vstup.
-- 2) Pokud vstup je prázdný, vypíše error (předpřipravený ve 'where' bloku)
-- 3) Jinak je na vstupu nějaké písmenko + zbytek.
-- 4) V takovém případě do stavu vloží nenaparsovaný zbytek a vrátí písmenko.
-- Použijte do-notaci -- dá se v ní používat i 'case _ of' ;)
parseAny :: Parser Char
parseAny = Parser $ do -- pracujeme v 'State' monádě, vnitřek má typ 'State String (Either ParseError Char)'
    input <- get
    case input of
        []         -> return $ Left expectedCharError
        (c : rest) -> State $ \s -> (rest, Right c)

{- Příklady:

>>> runState (runParser parseAny) ""
("",Left expected: any character, but found: end of file)

>>> runState (runParser parseAny) "ahoj"
("hoj",Right 'a')
-}

-- | Error, který hlásí že jsme čekali nějaký znak, ale našli jsme konec vstupu.
expectedCharError :: ParseError
expectedCharError = ParseError "any character" "end of file"

parseAnyTesty1 :: [Test (Either ParseError Char)]
parseAnyTesty1 =
    [ execParser parseAny ""
        === Left expectedCharError
        @@@ "execParser parseAny \" \" === Left expectedCharError"
    , execParser parseAny " "
        === Right ' '
        @@@ "execParser parseAny \" \" === Right ' '"
    ]

parseAnyTesty2 :: [Test String]
parseAnyTesty2 =
    [ finalStateParser parseAny ""
        === ""
        @@@ "finalStateParser parseAny \"\" === \"\""
    , finalStateParser parseAny "char"
        === "har"
        @@@ "finalStateParser parseAny \"char\" === \"har\""
    , finalStateParser parseAny " "
        === ""
        @@@ "finalStateParser parseAny \" \" === \"\""
    ]

-- TODO
-- Výsledek parseru se dá mapovat!
--
-- Mapování by mělo splňovat, že `mapParser id === id` a `mapParser f . mapParser g === mapParser (f . g)`.
-- Reálně byste v něm neměli měnit stav parseru (testy to kontrolují!)
mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f (Parser p) = Parser $ do -- do-notace je pro 'State String' monádu
    result <- p
    case result of
        Left  err -> return $ Left err
        Right res -> return $ Right (f res)

mapParserTesty1 :: [Test (Either ParseError Int)]
mapParserTesty1 =
    [ execParser (mapParser fourtyTwo parseEof)                    ""
        === Right 42
        @@@ "execParser (mapParser fourtyTwo parseEof) \"\" === Right 42"
    , execParser (mapParser addTwo (mapParser fourtyTwo parseEof)) ""
        === execParser (mapParser (addTwo . fourtyTwo) parseEof) ""
        @@@ "execParser (mapParser addTwo (mapParser fourtyTwo parseEof)) \"\" === execParser (mapParser (addTwo . fourtyTwo) parseEof) \"\""
    , execParser (mapParser fourtyTwo parseEof)                    "ahoj"
        === Left (ParseError "end of file" "a")
        @@@ "execParser (mapParser fourtyTwo parseEof) \"ahoj\" === Left (ParseError \"end of file\" \"a\")"
    ]
  where
    fourtyTwo _ = 42
    addTwo x = x + 2

mapParserTesty2 :: [Test String]
mapParserTesty2 =
    [ finalStateParser (mapParser fourtyTwo parseEof) "ahoj"
        === "ahoj"
        @@@ "finalStateParser (mapParser fourtyTwo parseEof) \"ahoj\" === \"ahoj\""
    , finalStateParser (mapParser fourtyTwo parseEof) ""
        === ""
        @@@ "finalStateParser (mapParser fourtyTwo parseEof) \"\" === \"\""
    ]
    where fourtyTwo _ = 42 :: Int

-- Tím pádem je 'Parser' funktor!
instance Functor Parser where
    fmap = mapParser

-- | Můžeme vyrobit parser, který se vstupem neudělá nic
-- a vrátí hodnotu, kterou dostal na vstupu.
returnParser :: a -> Parser a
returnParser x = Parser $ do
    return $ Right x

-- | Parser podporuje 'andThen' operaci.
-- Pokusíme se naparsovat parser v prvním argumentu.
-- Pokud uspěl, pustíme parser získaný funkcí (použijte 'runParser')
-- Pokud neuspěl, zpropagujeme error.
andThenParser :: Parser a -> (a -> Parser b) -> Parser b
andThenParser (Parser p) f = Parser $ do
    result <- p
    case result of
        Right x   -> runParser (f x)
        Left  err -> return $ Left err

-- Tím pádem je 'Parser' monáda!
instance Monad Parser where
    return = returnParser
    (>>=)  = andThenParser

-- Opět jen použijeme ten samý trik jako u 'State',
-- abychom nemuseli psát instanci pro 'Applicative' :)
instance Applicative Parser where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return $ f x

-----------------------------------------------------------------------------------
--                            2. ČÁST - Skládání                     [ 6 bodů ]   |
-----------------------------------------------------------------------------------

-- TODO: Operátor '<|>' (čteme jako _nebo_)
-- vezme dva parsery a spojí je dohromady.
-- 
-- Nejprve si uloží aktuální stav.
-- Pak pustí parser vlevo.
-- Pokud se povedl, vrátí výsledek.
-- Pokud se nepovedl, revertne stav a pustí druhý parser.
(<|>) :: Parser a -> Parser a -> Parser a
(Parser p1) <|> (Parser p2) = Parser $ do
    backup <- get
    res1   <- p1
    case res1 of
        Right x   -> return res1
        Left  err -> do
            set backup
            p2

-- | Pomocná funkce, která načte tři písmenka.
parseThreeLetters :: Parser String
parseThreeLetters = do
    x <- parseAny
    y <- parseAny
    z <- parseAny
    return [x, y, z]

{- Příklad:

>>> runState (runParser parseThreeLetters) "ahoj"  
("j",Right "aho")

>>> runState (runParser parseThreeLetters) "o"  
("",Left expected: any character, but found: end of file)
-}

orTesty1 :: [Test (Either ParseError String)]
orTesty1 =
    [ execParser (parseThreeLetters <|> eofToString parseEof) "a"
        === Left (ParseError "end of file" "a")
        @@@ "execParser (parseThreeLetters <|> eofToString parseEof) \"a\" === Left (ParseError \"end of file\" \"a\")"
    , execParser (parseThreeLetters <|> eofToString parseEof) "ahoj"
        === Right "aho"
        @@@ "execParser (parseThreeLetters <|> eofToString parseEof) \"ahoj\" === Right \"aho\""
    , execParser (parseThreeLetters <|> eofToString parseEof) ""
        === Right "<eof>"
        @@@ "execParser (parseThreeLetters <|> eofToString parseEof) \"\" === Right \"<eof>\""
    ]
    where eofToString = mapParser (\() -> "<eof>")

orTesty2 :: [Test String]
orTesty2 =
    [ finalStateParser (parseThreeLetters <|> eofToString parseEof) "a"
        === "a"
        @@@ "finalStateParser (parseThreeLetters <|> eofToString parseEof) \"a\" === \"a\""
    , finalStateParser (parseThreeLetters <|> eofToString parseEof) "ahoj"
        === "j"
        @@@ "finalStateParser (parseThreeLetters <|> eofToString parseEof) \"ahoj\" === \"j\""
    , finalStateParser (parseThreeLetters <|> eofToString parseEof) ""
        === ""
        @@@ "finalStateParser (parseThreeLetters <|> eofToString parseEof) \"\" === \"\""
    ]
    where eofToString = mapParser (\() -> "<eof>")

-- | Pomocná funkce, která vrátí parser, který nezkonzumoval vstup, ale selhal
--
-- Reálně jen tvoří 'ParseError'.
parseError :: String -> String -> Parser a
parseError expected found = Parser $ return $ Left $ ParseError expected found

-- TODO: funkce, která dostane popis toho co parsuje
-- a predikát, který označuje co chceme parsovat.
--
-- Pomocí 'parseAny' naparsuje nějaký znak
-- a pak se pomocí 'if _ then _ else _' zeptá jestli naparsovaný znak splňuje predikát.
-- Pokud ano, pak jej vrátí. Pokud ne, pak zavolá 'parseError' kde expected je 'description'
-- a actual je načtený znak.
--
-- Tahle funkce a ani žádná pozdější funkce již nepoužívá 'set' ani 'get' přímo!!!
satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description p = do -- do-notace pro 'Parser Char'
    c <- parseAny
    if p c then return c else parseError description [c]


satisfyTesty :: [Test (Either ParseError Char)]
satisfyTesty =
    [ execParser aParser "a"
        === Right 'a'
        @@@ "execParser aParser \"a\" === Right 'a'"
    , execParser aParser "baaa"
        === Left (ParseError "letter 'a'" "b")
        @@@ "execParser aParser \"baaa\" === Left (ParseError \"letter 'a'\" \"b\")"
    , execParser aParser "aaaa"
        === Right 'a'
        @@@ "execParser aParser \"aaaa\" === Right 'a'"
    ]
    where aParser = satisfy "letter 'a'" (== 'a')

-- | Tato pomocná funkce spustí parser na zadaný string
-- a vrátí buď naparsovanou hodnotu, či parse error
--
-- Počítá s tím, že po naparsování zbyde prázdný string (volá 'parseEof') na konci.
run :: Parser a -> String -> Either ParseError a
run p s = snd $ runState (runParser go) s
  where
    go = do -- do-notace pro 'Parser a'
        result <- p
        parseEof
        return result

-- TODO: Pomocí 'satisfy' naprogramujte parser,
-- který naparsuje pouze zadaný znak.
-- Jako description dejte jen samotný znak, který chcete parsovat.
char :: Char -> Parser Char
char c = satisfy [c] (== c)

-- Testovací parser, který naparsuje string "ahoj"
-- Níže je pak 'string', který umí naparsovat string rovnou ;)
parseAhoj :: Parser String
parseAhoj = do
    _ <- char 'a'
    _ <- char 'h'
    _ <- char 'o'
    _ <- char 'j'
    return "ahoj"

charTesty :: [Test (Either ParseError String)]
charTesty =
    [ execParser parseAhoj "ahoj"
        === Right "ahoj"
        @@@ "execParser parseAhoj \"ahoj\" === Right \"ahoj\""
    , execParser parseAhoj "abba"
        === Left (ParseError "h" "b")
        @@@ "execParser parseAhoj \"abba\" === Left (ParseError \"h\" \"b\")"
    , execParser parseAhoj ""
        === Left expectedCharError
        @@@ "execParser parseAhoj \"\" === Left expectedCharError"
    ]

-- | Parser pro whitespace
space :: Parser Char
space = satisfy "space" isSpace

-- | Parser pro cifru
digit :: Parser Char
digit = satisfy "digit" isDigit

-----------------------------------------------------------------------------------
--                   3. ČÁST - Větší a větší parsery                 [ 6 bodů ]   |
-----------------------------------------------------------------------------------

-- | Dostane parser a pokusí se jej naparsovat několikrát.
-- (naparsuje 0..nekonečno-krát)
-- 
-- Používá vzájemnou rekurzi s 'many1'.
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- | Dostane parser a pokusí se jej naparsovat několikrát, ale alespoň jednou.
-- (naparsuje 1..nekonečno-krát)
--
-- Používá vzájemnou rekurzi s 'many'.
many1 :: Parser a -> Parser [a]
many1 p = do -- tady je do-notace pro 'Parser [a]'
    first <- p
    rest  <- many p
    return (first : rest)

-- TODO: Pomocí 'char' a 'mapM :: Monad b => (a -> m b) -> [a] -> m [b]' vyrobte parser,
-- který naparsuje přesně zadaný řetězec.
string :: String -> Parser String
string = mapM char

stringTesty :: [Test (Either ParseError String)]
stringTesty =
    [ execParser (string "ahoj") "ahoj"
        === Right "ahoj"
        @@@ "execParser (string \"ahoj\") \"ahoj\" === Right \"ahoj\""
    , execParser (string "ahoj") "abba"
        === Left (ParseError "h" "b")
        @@@ "execParser (string \"ahoj\") \"abba\" === Left (ParseError \"h\" \"b\")"
    , execParser (string "ahoj") ""
        === Left expectedCharError
        @@@ "execParser (string \"ahoj\") \"\" === Left expectedCharError"
    ]

-- TODO: Pomocí 'many1', 'digit' a 'read :: String -> Int' vyrobte parser,
-- který naparsuje číslo.
number :: Parser Int
number = do -- do-notace pro Parser
    res <- many1 digit
    return $ read res

numberTesty :: [Test (Either ParseError Int)]
numberTesty =
    [ run number "123" === Right 123 @@@ "run number \"123\" === Right 123"
    , run number "123ab"
        === Left (ParseError "end of file" "a")
        @@@ "run number \"123ab\" === Left (ParseError \"end of file\" \"a\")"
    , run number ""
        === Left expectedCharError
        @@@ "run number \"\" === Left expectedCharError"
    ]

-- | Naparsuje vícero mezer (nebo žádnou).
--
-- Používá se k snězení zbytečného whitespacu.
spaces :: Parser String
spaces = many space

-- | 'symbol' naparsuje daný string
-- a za ním naparsuje nula až nekonečno mezer.
--
-- Využijte 'string' a 'spaces'.
symbol :: String -> Parser String
symbol s = do
    result <- string s
    _      <- spaces
    return result

-- | Naparsuje vlevo, vpravo a uprostřed
-- a vrátí naparsované uprostřed.
--
-- Tohle se hodí pro závorky a podobné věci!
between :: Parser a -> Parser c -> Parser b -> Parser b
between left right p = do
    _      <- left
    result <- p
    _      <- right
    return result

-- TODO: pomocí 'between' a 'symbol' vyrobte parser,
-- který naparsuje věci mezi hranatými závorkami
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

bracketsTesty :: [Test (Either ParseError String)]
bracketsTesty =
    [ run (brackets parseAhoj) "[]"
        === Left (ParseError "a" "]")
        @@@ "run (brackets parseAhoj) \"[]\" === Left (ParseError \"a\" \"[\")"
    , run (brackets parseAhoj) "[ahoj]"
        === Right "ahoj"
        @@@ "run (brackets parseAhoj) \"[ahoj]\" === Right \"ahoj\""
    , run (brackets parseAhoj) "[abba]"
        === Left (ParseError "h" "b")
        @@@ "run (brackets parseAhoj) \"[abba]\" ===  Left (ParseError \"h\" \"b\")"
    , run (brackets parseAhoj) "[ahoj"
        === Left expectedCharError
        @@@ "run (brackets parseAhoj) \"[ahoj\" ===  Left expectedCharError"
    ]

-- | Naparsuje "null", pokud uspěje, vrátí ().
parseNull :: Parser ()
parseNull = do
    _ <- symbol "null"
    return ()

-- TODO: Naparsuje nějaký řetězec vymezený uvozovkami.
-- Využijte 'between', 'many' a 'parseNonquoteChar'.
parseQuotedString :: Parser String
parseQuotedString = between (symbol "\"")
                            (symbol "\"")
                            (many parseNonquoteChar)
    where parseNonquoteChar = satisfy "not a \"" (/= '"') -- the text of satisfy should never happen

{- Příklady:
>>> run parseQuotedString "\"\""
Right ""
>>> run parseQuotedString "\"\"\""
Left expected: end of file, but found: "
>>> run parseQuotedString "\"ahoj\""
Right "ahoj"
-}

parseQuotedStringTesty :: [Test (Either ParseError String)]
parseQuotedStringTesty =
    [ run parseQuotedString "\"\""
        === Right ""
        @@@ "run parseQuotedString \"\\\"\\\"\" === Right \"\\\"\"\""
    , run parseQuotedString "\"\"\""
        === Left (ParseError "end of file" "\"")
        @@@ "run parseQuotedString \"\\\"\\\"\\\"\" === Left (ParseError \"end of file\" \"\\\"\")"
    , run parseQuotedString "\"ahoj\""
        === Right "ahoj"
        @@@ "run parseQuotedString \"\\\"ahoj\\\"\" ===  Right \"ahoj\""
    , run parseQuotedString "\"long string with spaces\""
        === Right "long string with spaces"
        @@@ "run parseQuotedString \"\\\"long string with spaces\\\"\" ===  Right \"long string with spaces\""
    , run parseQuotedString "\"not closed, oh no"
        === Left expectedCharError
        @@@ "run parseQuotedString \"\\\"not closed, oh no\" === Left expectedCharError"
    ]
-----------------------------------------------------------------------------------
--                            4. ČÁST - JSON                         [ 4 body ]   |
-----------------------------------------------------------------------------------

-- | Funkce 'sepBy' a 'sepBy1' trochu připomínají 'many' a 'many1'.
-- Vezmou parser 'a'ček a parser 'sep'arátorů
-- a mezi každým načteným 'a'čkem načtou i 'sep'arátor.
sepBy, sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy p s = sepBy1 p s <|> return []
sepBy1 p s = do
    first <- p
    rest  <- many (s >>= \_ -> p) -- stručněji jako 'many (s >> p)'
    return (first : rest)

-- TODO: Vezme parser 'p' a naparsuje seznam, který vypadá jako "[_, _, _]",
-- kde každý prvek je naparsován pomocí 'p'.
--
-- Využije 'brackets' a 'sepBy'.
parseListOf :: Parser a -> Parser [a]
parseListOf p = brackets $ p `sepBy` symbol ","

parseListOfTesty :: [Test (Either ParseError [Int])]
parseListOfTesty =
    [ run (parseListOf number) "[]"
        === Right []
        @@@ "run (parseListOf number) \"[]\" === Right []"
    , run (parseListOf number) "[1]"
        === Right [1]
        @@@ "run (parseListOf number) \"[1]\" === Right [1]"
    , run (parseListOf number) "[1,2,3]"
        === Right [1, 2, 3]
        @@@ "run (parseListOf number) \"[1,2,3]\" === Right [1,2,3]"
    , run (parseListOf number) "[1,2,3,]"
        === Left (ParseError "]" ",")
        @@@ "run (parseListOf number) \"[1,2,3,]\" === Left (ParseError \"]\" \",\")"
    , run (parseListOf number) "[,]"
        === Left (ParseError "]" ",")
        @@@ "run (parseListOf number) \"[,]\" === Left (ParseError \"]\" \",\")"
    , run (parseListOf number) "1"
        === Left (ParseError "[" "1")
        @@@ "run (parseListOf number) \"1\" === Left (ParseError \"[\" \"1\")"
    , run (parseListOf number) ""
        === Left expectedCharError
        @@@ "run (parseListOf number) \"\" === Left expectedCharError"
    ]


{- Příklady:
>>> run (parseListOf number) "[]"
Right []
>>> run (parseListOf number) "[1,2]"
Right [1,2]

>>> run (parseListOf parseQuotedString) "[\"Ahoj\",\"svete\"]"
Right ["Ahoj","svete"]
-}

-- Nakonec se nám hodí ještě jeden kombinátor,
-- který vezme seznam parserů a postupně na ně
-- bude aplikovat (<|>):
choice :: String -> [Parser a] -> Parser a
choice desc = foldr (<|>) noMatch where noMatch = parseError desc "no match"

-- | Datový typ pro hodnoty, které budeme chtít naparsovat:
data Value
  = ValueNull -- null
  | ValueString String -- "řetězec"
  | ValueNumber Int -- 123456
  | ValueBool Bool -- true / false
  | ValueList [Value] -- [_, _, _]
  deriving (Eq, Show)

-- TODO: Napište parser pro booleovské hodnoty:
-- * Vyrobte parser, který po načtení 'true' vrátí haskellové True
-- * Vyrobte parser, který po načtení 'false' vrátí haskellové False
--
-- * Následně použijte '<|>' či 'choice' pro zkombinování těchto dvou parserů.
parseBool :: Parser Bool
parseBool = parseTrue <|> parseFalse
  where
    parseTrue = do 
        symbol "true"
        return True
    parseFalse = do
        symbol "false"
        return False

parseBoolTesty :: [Test (Either ParseError Bool)]
parseBoolTesty =
    [ run parseBool "true"
        === Right True
        @@@ "run parseBool \"true\" === Right True"
    , run parseBool "false"
        === Right False
        @@@ "run parseBool \"false\" === Right False"
    ]

{- Syntaktická poznámka:

`fmap` se často píše pomocí operátoru `<$>`.
Tedy `fmap f x` je `f <$> x`.
Můžete si to představit jako aplikace funkce `$`, ale v nějakém kontejneru.

-}

-- | Hezký parser pro 'Value'
parseValue :: Parser Value
parseValue = do
    _ <- spaces
    choice
        "list of values or string or number or bool or null"
        [ ValueList <$> parseListOf parseValue
        , ValueString <$> parseQuotedString
        , ValueBool <$> parseBool
        , ValueNumber <$> number
        , (\_ -> ValueNull) <$> parseNull -- Poznámka: Tady se obvykle používá operátor `<$`, který je definován jako `const fmap`
        ]

{- Příklady:

>>> run parseValue ""
Left expected: list of values or string or number or bool or null, but found: no match
>>> run parseValue "[]"
Right (ValueList [])
>>> run parseValue "[1, 2, 3, \"ahoj\"]"
Right (ValueList [ValueNumber 1,ValueNumber 2,ValueNumber 3,ValueString "ahoj"])

>>> run parseValue "[1, 2, [null, null, null], \"ahoj\"]"
Right (ValueList [ValueNumber 1,ValueNumber 2,ValueList [ValueNull,ValueNull,ValueNull],ValueString "ahoj"])

>>> run parseValue "[[[[null]]]]"
Right (ValueList [ValueList [ValueList [ValueList [ValueNull]]]])
>>> run parseValue "            42"
Right (ValueNumber 42)
-}

-- BONUS: Přidejte 'ValueObject', která popisuje JSONovitý objekt
-- a parser pro něj -- ten by se měl vejít do deseti řádek.

-----------------------------------------------------------------------------------
--                             HERE BE DRAGONS                                    |
----------------------------------------------------------------------------------
--
-- Níže následuje malinkatý framework pro unit testy, který jsem napsal,
-- abyste si mohli otestovat svůj kód. :)
-- Zavolejte 'main' v GHCi a vypíše se vám hezký přehled.
--
-- Kód níže samozřejmě můžete zkoumat a upravovat, odevzdávat jej nemusíte... ;)
--
-----------------------------------------------------------------------------------

-- | A 'Test' is a pair of (expected value, actual value)
-- together with an optional description
data Test a = Test
    { expected    :: a
    , actual      :: a
    , description :: Maybe String
    }
    deriving (Show, Eq)

-- | A binary operator for creating a basic test without a description
--
-- Example:
-- >>> 2 + 8 === 10
(===) :: a -> a -> Test a
actualValue === expectedValue = Test expectedValue actualValue Nothing

-- | A binary operator for annotating a test with a description
--
-- Example:
-- >>> 2 + 8 === 10 @@@ "Two plus eight should be ten!"
(@@@) :: Test a -> String -> Test a
test @@@ desc = test { description = Just desc }

-- | Gets a description of a 'Test'.
--
-- Returns @expected === actual@ if the given test has no description.
getTestDescription :: Show a => Test a -> String
getTestDescription t = case description t of
    Just someDescription -> someDescription
    Nothing              -> show (expected t) <> " === " <> show (actual t)

-- | A 'TestResult' is either 'OK', 'Fail' or 'FailException'
data TestResult
  = OK
  | Fail
  | FailException SomeException
  deriving (Show)

-- | Takes a list of 'TestResult' and returns a pair of numbers,
-- where the first number is the number of 'OK's
-- and the second number is the number of 'Fail's
sumTestResults :: [TestResult] -> (Int, Int)
sumTestResults = foldr go (0, 0)
  where
    go OK                (oks, fails) = (oks + 1, fails)
    go Fail              (oks, fails) = (oks, fails + 1)
    go (FailException _) (oks, fails) = (oks, fails + 1)

-- | Runs a test producing a 'TestResult'
runTest :: Eq a => Test a -> TestResult
runTest t | expected t == actual t = OK
          | otherwise              = Fail

-- | Takes a 'Test' and its 'TestResult' and produces a 'String'
-- with details about the test and its success/failure
describeTest :: Show a => Test a -> TestResult -> String
describeTest t OK   = getTestDescription t <> " ... OK "
describeTest t Fail = unlines
    [ getTestDescription t <> " ... FAIL"
    , "    " <> "Expected: " <> show (expected t)
    , "    " <> "Actual:   " <> show (actual t)
    ]
describeTest t (FailException e) = unlines
    [getTestDescription t <> " ... FAIL with exception", "    " <> show e]

-- | Takes a list of 'Test a', runs it and returns a single 'String'
-- describing the result and a pair of two 'Int's -- number of 'OK' and number of 'Fail' resp.
runTests :: (Eq a, Show a) => [Test a] -> (String, (Int, Int))
runTests tests = (resultsString, resultsSum)
  where
    results       = catchPure . runTest <$> tests
    resultsSum    = sumTestResults results
    resultsString = unlines $ zipWith describeTest tests results
    catchPure v = unsafePerformIO $ evaluate v `catch` failOnException

    failOnException :: SomeException -> IO TestResult
    failOnException = pure . FailException

-- | The main entrypoint to a Haskell module
main :: IO ()
main = do
    hSetEncoding stdout utf8

    putStrLn "Testing..."
    putStrLn ""

    testPart1
    testPart2
    testPart3
    testPart4

testPart1 :: IO ()
testPart1 = do
    hSetEncoding stdout utf8
    runTestGroup "parseAny - exec"         parseAnyTesty1
    runTestGroup "parseAny - final state"  parseAnyTesty2
    runTestGroup "mapParser - exec"        mapParserTesty1
    runTestGroup "mapParser - final state" mapParserTesty2

testPart2 :: IO ()
testPart2 = do
    hSetEncoding stdout utf8
    runTestGroup "<|> - exec"        orTesty1
    runTestGroup "<|> - final state" orTesty2
    runTestGroup "satisfy"           satisfyTesty
    runTestGroup "char"              charTesty

testPart3 :: IO ()
testPart3 = do
    hSetEncoding stdout utf8
    runTestGroup "string"            stringTesty
    runTestGroup "number"            numberTesty
    runTestGroup "brackets"          bracketsTesty
    runTestGroup "parseQuotedString" parseQuotedStringTesty

testPart4 :: IO ()
testPart4 = do
    hSetEncoding stdout utf8
    runTestGroup "parseListOf" parseListOfTesty
    runTestGroup "parseBool"   parseBoolTesty

-- | A helper function to run a group of tests
-- with a pretty name and a summary
runTestGroup :: (Show a, Eq a) => String -> [Test a] -> IO ()
runTestGroup name tests = do
    putStrLn $ "=== " <> name <> " ==="
    let (str, (oks, fails)) = runTests tests
    let total               = oks + fails
    putStrLn str
    putStrLn $ unwords
        [ show oks <> "/" <> show total <> " ... OK,"
        , show fails <> "/" <> show total <> " ... FAIL"
        ]
    putStrLn ""
