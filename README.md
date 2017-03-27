# FLDigi

## Note that this documentation almost always lags behind the actual code (because writing code is more fun than writing docs).

This will eventually be a library for remote-controlling FLDigi and your HF rig from CL. While it does work at the time of this writing, there's just enough code here to prove that this will work. I intend to model this on my fldigi-ruby library, found here:  https://github.com/jfrancis42/fldigi-ruby

For the moment, there's few useful things you can do, but it proves the XMLRPC works correctly. You can set and get the frequency and the carrier, for example:

````
(fldigi:set-frequency 14070000)
````

Do not rely on what little is here, it's almost certain that function names, etc will change. Just a place-holder at the moment.
