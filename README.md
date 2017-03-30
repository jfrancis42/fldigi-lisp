# FLDigi

## Note that this documentation almost always lags behind the actual code (because writing code is more fun than writing docs).

This is a library for remote-controlling FLDigi and your HF rig from CL. The library is not quite 100% complete, however, it's complete enough to work and be useful. I have modeled this on my fldigi-ruby library, found here:  https://github.com/jfrancis42/fldigi-ruby

Here's an example. We'll set the carrier frequency to 1000hz, set the radio dial frequency to 14.070 mhz, set the mode to BPSK31, then send a test message:

````
(fldigi:set-modem "BPSK31")
(fldigi:set-frequency 14070000)
(fldigi:set-carrier 1000)
(fldigi:send-buffer "test test test de n0body k")
````

More to come, but thus far, it seems to work pretty well.
