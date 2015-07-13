# General information
This repository contains my old assembly projects. They are written for MASM x86 assemblers with Intel syntax.
  
Year of creation: **2011**

# Projects
## ASCII Art (RSA key fingerprint)
The program parses RSA key fingerprint passed as input and prints its [ASCII Art visualization](https://blog.rootshell.be/2008/07/15/ssh-fingerprint-ascii-visualization/).
### Usage
`ascii_art 0 <rsa-key-fingerprint>`
Example: `ascii_art 0 d5:29:3e:9a:8d:90:26:5d:6b:6b:fb:8a:bb:a5:da:23`

## Base 64
Base64 file encoder / decoder.
### Usage
##### Encoding
`base64 <input-file> <output-file>`
##### Decoding
`base64 -d <input-file> <output-file>`

## Koch Curve fractal
The program uses basic VGA graphic mode for drawing Koch Curve fractal based on user input.
### Usage
`koch_curve <number-of-iterations> <line-length>`
Example: `koch_curve 3 10`