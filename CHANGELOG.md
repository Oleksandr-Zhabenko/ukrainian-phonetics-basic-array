# Revision history for ukrainian-phonetics-basic-array

## 0.1.0.0 -- 2020-12-14

* First version. Released on an unsuspecting world. A fork of the ukrainian-phonetics-basic package that transformed into
an independent one. All the vector-related functionality has been removed.

## 0.1.1.0 -- 2020-12-18

* First version revised A. Changed the dependency boundaries to fix the array usage issue. Please, update to the new
version to avoid the misusage.

## 0.1.2.0 -- 2021-01-27

* First version revised B. Added new variants of the sound representation durations to the Languages.Phonetic.Ukrainian.Syllable.Double.Arr and Languages.Phonetic.Ukrainian.Syllable.Arr modules.
Some documentation improvements.

## 0.2.0.0 -- 2021-10-30

* Second version. Switched to the Case.Hashable.Cuckoo.getBFstL' function where possible. Updated the dependencies.

## 0.3.0.0 -- 2021-10-31

* Third version. Switched back to the CaseBi.Arr.getBFstLSorted' functionality.

## 0.4.0.0 -- 2021-11-03

* Fourth version. Added Int8-based optimized functionality.The modules that do not use Int8 after ugrading the
phonetic-languages-simplified-examples-array to the Int8-based functionality are planned to be moved to a new package.

## 0.4.1.0 -- 2021-11-04

* Fourth version revised A. Some documentation improvements. Moved the new modules with Int8 functionality without ByteString
related functionality to the new location path without ByteString folder.

## 0.4.2.0 -- 2021-11-05

* Fourth version revised B. Removed the inlining in the new modules to make them similar to the previous ones. Probably it
led to not optimized compiling of the further code using this ones.

## 0.5.0.0 -- 2022-02-19

* Fifth version. Some code optimizations and improvements (mostly) in speed. Moved the functionality to the 2 new packages: 
ukrainian-phonetics-common and ukranian-phonetics-basic-array-bytestring. Added new tuples-based functionality.

## 0.5.1.0 -- 2022-02-21

* Fifth version revised A. Fixed issues with the similarly defined sDurat4 and sDurat3 functions. Some code improvements. 
Added extended versions of the functions to the Languages.Phonetic.Ukrainian.Syllable.ArrInt8 module.

## 0.5.2.0 -- 2022-02-21

* Fifth version revised B. Changed the policy of the package dependencies boundaries. Added several functions that are needed further to 
the export list.

## 0.5.3.0 -- 2022-03-24

* Fifth version revised C. Updated the dependencies boundaries so that the latest versions of GHC and Cabal are supported.

## 0.6.0.0 -- 2022-08-09

* Sixth version. Fixed issues with the syllable segmentation with soft sign consonant sequences. Changed the structure of the non-sound Ukraninian symbols
representation (now they are converted to one of the [100,101,102]). 
Fixed some issues with the dash '-' and apostrophe signs in word and syllable segmentation.
These changes affect the general results of the package functions work.

## 0.7.0.0 -- 2023-02-02

* Seventh version. Switched to NoImplicitPrelude and Strict extensions. Updated the metadata and dependencies boundaries. Changed the names of the modules.

## 0.7.1.0 -- 2024-02-22

* Seventh version revised A. Some code improvements related to inlining. Switched to intermediate-structures instead of mmsyn5 as a dependency. Added README.md, devotion and Github repository with bug-tracker.

## 0.7.1.1 -- 2024-02-22

* Seventh version revised B. Fixed issue with documentation link.


