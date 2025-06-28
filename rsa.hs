import System.Random 
import GHC.IO.Encoding.Latin1 (ascii)
import System.Environment
import Data.Char (ord)
import GHC.Char  

--implementation of https://en.wikipedia.org/wiki/Carmichael_function
--actually this can be replaced with just (p-1)*(q-1) as mentioned on the wiki
carmichael_function :: Integer -> Integer -> Integer
carmichael_function p q = lcm (p - 1) (q - 1)
    where 
        lcm a b = abs(a*b) `div` gcd a b
--implementation of https://en.wikipedia.org/wiki/Exponentiation_by_squaring
exponentiation_by_squaring :: Integer -> Integer -> Integer -> Integer
exponentiation_by_squaring _ 0 _ = 1
exponentiation_by_squaring x n m 
    | n `mod` 2 == 1 = (x * exponentiation_by_squaring ((x * x) `mod` m) ((n-1) `div` 2) m) `mod` m  
    | otherwise = (exponentiation_by_squaring ((x*x) `mod` m) (n `div` 2) m )  `mod` m


inverse :: Integer -> Integer -> Integer
inverse e t = 
    let x = go e t 0 1
    in if x < 0 then x + t else x
    where
        go 1 _ _ s' = s'
        go r' r s s' = 
            let quitent  = r `div` r'
            in go (r - quitent*r') r'  s' (s - quitent*s')


decompose n = go 0 (n-1) 
    where 
        go s d
            | d `mod` 2 == 0 = go (s+1) (d `div` 2)
            | s == 0 = go s (d-1)
            | otherwise = (s,d)

--implementation of miller's primarilty test https://en.wikipedia.org/wiki/Miller–Rabin_primality_test
millerTest :: Integer -> Int -> StdGen -> Bool
millerTest n k g = 

    let (s, d) = decompose n
        samples = take k $ randomRs (2, n - 2) g
        test :: Integer -> Bool
        test  a =
            let x = exponentiation_by_squaring a d n
                loop r x
                    | r == 0 = y == 1
                    | y == 1 && x /= 1 && x /= n-1 = False
                    | otherwise  = loop (r - 1) (y)
                    where y = (x * x) `mod` n
            in loop (s-1) x

    in all test samples 

asciToNum :: String -> Integer
asciToNum = foldl (\acc c -> acc * 256 + toInteger (ord c)) 0

numToasci :: Integer -> String
numToasci 0 = ""
numToasci n = chr (fromInteger n `mod` 256) : numToasci (n `div` 256)

getChunks :: Integer -> String -> [String]
getChunks _ [] = []
getChunks n s = 
    let (chunk,rest) = splitAt (fromInteger n) s
    in chunk : getChunks n rest

encrypt :: String -> (Integer,Integer,Integer) -> String
encrypt fileContent (e,n,blockSize) = 
    let fileLines = lines fileContent 
    in unlines (map encryptLine fileLines)
    where
        encryptLine line = 
            let blocks = getChunks blockSize line 
            in unwords (map (\b -> show (exponentiation_by_squaring (asciToNum b) e n)) blocks) 

decryptBlock :: String -> (Integer,Integer) -> String
decryptBlock b (d,n) =
    let num = read b :: Integer
        res = exponentiation_by_squaring num d n
    in numToasci res
 
decrypt :: String -> (Integer,Integer) -> String
decrypt input (d,n) = 
    let fileLines = lines input
    in unlines (map (reverse . decryptLine) fileLines)
    where
        decryptLine line = 
            let blocks = words line
            in concatMap (\b -> numToasci (exponentiation_by_squaring (read b) d n)) blocks

    
   
--RSA's Key generation part where (e,n) is the public key and (d,n) the private key
generateKey :: Integer -> StdGen -> (Integer,Integer,Integer)
generateKey keyLength g =
    let len_p = keyLength `div` 2
        len_q = keyLength - len_p
        xs = [x | x <- randomRs (2^len_p,2^(len_p+1)-1) g]
        p = head (filter (\x -> millerTest x 10 g) xs)
        ys = [x | x <- randomRs (2^len_q,2^(len_q+1)-1) g,x/=p]
        q = head (filter (\x -> millerTest x 10 g) ys)
        n = p*q
        e = 65537
        t = carmichael_function p q
        d = inverse e t
    in (n,d,e)

readInput :: String -> String -> IO (Integer,Integer,String)
readInput keyFile inputFile = do
    keyString <- readFile keyFile
    inputString <- readFile inputFile
    --fmap (take 2 . lines) (readFile keyString)
    let (e,k) = case lines keyString of 
            (a : b : _) -> (read a, read b)
            _ -> error "File Format is invalid"
    return (e,k,inputString)


bits :: Integer -> Integer
bits 0 = 0
bits n = 1 + bits (n `div` 2)
main = do
    args <- getArgs
    case args of 
        ["-gen-key", keyLen,"-name", name] -> do
            g <- initStdGen
            let (n,d,e) = generateKey (read keyLen :: Integer) g
                pubFile = "Pub_" ++ name ++ ".key"
                privFile = "Priv_" ++ name ++ ".key"
            writeFile pubFile (unlines [show e,show n])
            writeFile privFile (unlines [show d,show n])
            putStrLn "Sucessfully generated keys."
        ["-gen-key", keyLen] -> do
            g <- initStdGen
            let (n,d,e) = generateKey (read keyLen :: Integer) g
                pubFile = "Pub.key"
                privFile = "Priv.key"
            writeFile pubFile (unlines [show e,show n])
            writeFile privFile (unlines [show d,show n])
            putStrLn "Sucessfully generated keys." 

        ["-encrypt",keyFile,inputFile,"-o", name] -> do
            (e,n,input) <-  readInput keyFile inputFile
            writeFile (name ++ ".txt") (unlines [encrypt input (e,n, (bits n) `div` 8 )])
        ["-encrypt",keyFile,inputFile] -> do
            (e,n,input) <-  readInput keyFile inputFile
            writeFile ("output.txt") (unlines [encrypt input (e,n, (bits n) `div` 8 )])

        ["-decrypt",keyFile,inputFile,"-o", name] -> do
            (d,n,input) <-  readInput keyFile inputFile
            writeFile (name ++ ".txt") (unlines  [decrypt input (d,n)])
        ["-decrypt",keyFile,inputFile] -> do
            (d,n,input) <-  readInput keyFile inputFile
            putStrLn (decrypt input (d,n))
        
        _ -> putStrLn $ "Incorrect Command, Please make sure it is one of the following:\n\n" ++
                      "runhaskell rsa.hs -gen-key [n] -name [KeyName]\n" ++
                      "runhaskell rsa.hs -gen-key [n]\n" ++
                      "runhaskell rsa.hs -encrypt [keyFileName] [toEncryptFileName] -o [encryptedFileName]\n" ++
                      "runhaskell rsa.hs -encrypt [keyFileName] [toEncryptFileName]\n" ++
                      "runhaskell rsa.hs -decrypt [keyFileName] [toDecryptFileName] -o [decryptedFileName]" ++
                      "runhaskell rsa.hs -encrypt [keyFileName] [toDecryptFileName]"

    