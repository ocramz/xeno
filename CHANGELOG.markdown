    0.4
    * A number of optimizations and some changes in ergonomics. Thanks to Dmitry Krylov (dmalkr) and Michal Gajda (mgajda) !
    * breaking API changes : 
        * The parameters to function 'Xeno.SAX.process' are now wrapped in a Process type
    * Speed optimizations : 
        * function 'Xeno.DOM.predictGrowSize' 
        * Xeno.DOM.Robust
    * Benchmark improvements : 
        * Added benchmarks for ByteStringZeroTerminated
        * Added benchmarks for big files (bench/SpeedBigFiles.hs)
        * Benchmarks run non-threaded

    0.3.5.2
	* Fix dependency lower bounds (GHC 8.0.1 is the earliest version currently supported)

	0.3.5
	* Improve error handling (#24 #26, mgajda)

	0.3.4
	* Fixed #14 and add test for #15
	* Fixed typos in the examples (unhammer)

	0.3.2
	* Fixed DOM parsing from bystrings with non-zero offset (#11, qrilka)
	
	0.3
	* Fixed name parsing (for attributes and tags) so it conforms with the XML spec (qrilka)
	* Fixed parsing failure when root tag is preceded by white space (though without checking for white space characters specifically) (qrilka)
	* Added contribution guidelines (ocramz)	

	0.2
	* Added CDATA support (Rembane)	

	0.1
	* First Hackage release
