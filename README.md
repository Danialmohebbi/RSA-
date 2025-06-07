## RSA encryption algorithm 
RSA is one of the oldest and most commonly used public-key cryptosystem.
it uses two keys which are public and private keys and distinct.
the security of RSA is due to the diffculty of factoring the primes generated in 
[Key generation](https://en.wikipedia.org/wiki/RSA_cryptosystem#Key_generation) which are used in the encryption and decryption of the data.


## Possible Commands
1. Key generation
  ```bash
  runhaskell rsa.hs -gen-key [n] -name [KeyName]
  runhaskell rsa.hs -gen-key [n] 
  ```
2. Encryption
  ```bash
  runhaskell rsa.hs -encrypt [keyFileName] [toEncryptFileName] -o [decryptedFileName]
  runhaskell rsa.hs -encrypt [keyFileName] [toEncryptFileName]
  ```
3. Decryption
  ```bash
   runhaskell rsa.hs -decrypt [keyFileName] [toDecryptFileName] -o [decryptedFileName]
   runhaskell rsa.hs -encrypt [keyFileName] [toDecryptFileName]
```
where in `(1)`,`-o` will make keys named `Pub_{KeyName}.Key` and `Priv_{KeyName}.Key` instead of the default `Pub.key` and `Priv.Key`.
      in `(2)` and `(3)` it will outputs the result to a fileName.txt if `-o` is given and if not, in `(2)` it will output it to output.txt while in `(3)` it will be outputed to the terminal.
## An Example on How to Use

1. **Clone the repository**:
   First, clone the repository to your local machine using the following command:

   ```bash
   git clone <repository-url>   
   ```
2. **Generating the key**
   ```bash
   runhaskell rsa.hs -gen-key 12 -name week_7
   ```
   now i have the keys
   ```
   Priv_week_7.key
   Pub_week_7.key
   ```
3. **Encryption**
   ```bash
   runhaskell rsa.hs -encrypt Pub_week_7.key input.txt -o nice
   ```
   encrypting the input.txt into a file named nice.txt
4. **Decryption**
   ```bash
   runhaskell rsa.hs -decrypt Priv_week_7.key nice.txt 
   ```
   which will output
   ```
   kittens are the best emotional support budies
   ```

---
