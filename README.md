# LesHouches

Wolfram Language package providing import rules and conversion for Supersymmetric Les Houches Accord (SLHA) files

The conversion function is fast, native to the Wolfram Language, and allows partial import of a wide variety SLHA file elements. This package is compatible with both SLHA1 ([arXiv:hep-ph/0311123](https://arxiv.org/abs/hep-ph/0311123)) and SLHA2 ([arXiv:hep-ph/0801.0045](https://arxiv.org/abs/0801.0045)) standards, as well as the proposed [cross section extension](https://phystev.cnrs.fr/wiki/2013:groups:tools:slha) (although the associated features have not been seriously tested).


**Code:** [github.com/statius/leshouches](https://github.com/statius/leshouches)

## Installation

- Download the latest release and unpack it or clone the repository somewhere on the Wolfram Language `$Path` (e.g. the `Applications` folder in `$UserBaseDirectory` for *Mathematica*).

- Load *LesHouches* as

  ```mathematica
  Needs @ "LesHouches`"
  ```
  This will register the import converter in the current session.

- The import converter can be configured for automatic registration in all sessions by installing it in the `$UserBaseDirectory` file tree:

  ```mathematica
  InstallLesHouches @ "Converters"
  ```
  
  The installed files reference the absolute file path to the *LesHouches* package, so if the location of the package directory changes, `InstallLesHouches` will need to be run again. 
  
  To remove the converter, run
  
  ```mathematica
  UninstallLesHouches @ "Converters"
  ```

## Usage

### Import elements
- General `Import` elements:

  | element name | expression form                                         |
  | ------------ | ------------------------------------------------------- |
  | "Elements"   | list of all elements and options available in this file |
  | "Rules"      | full list of rules for each element and option          |
  | "Options"    | list of rules for options, properties, and settings     |
  | "Summary"    | Summary `Dataset` of data element names and counts      |

- Data representation elements:

  | element name | expression form                                         |
  | ------------ | ------------------------------------------------------- |
  | "Blocks"     | list of spectrum data blocks as `LesHouchesDataObject`s |
  | "Decays"     | list of decay tables as `LesHouchesDataObject`s         |
  | "XSections"  | list of cross section tables as `LesHouchesDataObject`s |
  
- Metadata  elements:

  | element name        | expression form                            |
  | ------------------- | ------------------------------------------ |
  | "BlockNames"        | list of spectrum data blocks names         |
  | "DecayStates"       | list of decay tables initial states        |
  | "XSectionProcesses" | list of cross section tables process rules |

- Subelements of data representation elements, in the form {*elem*, *subelem*}:

  | element name | subelements                          |
  | ------------ | ------------------------------------ |
  | "Blocks"     | "Elements", "Rules", "Names"         |
  | "Decays"     | "Elements", "Rules", "InitialStates" |
  | "XSections"  | "Elements", "Rules", "ProcessRules"  |

- Specific blocks, decay tables, or cross sections in the form {*elem*, *id*}, where

  | element name | identification                                      |
  | ------------ | --------------------------------------------------- |
  | "Blocks"     | block name string or list of strings                |
  | "Decays"     | decay parent particle PDG number or list of numbers |
  | "XSections"  | sequence of one or both of the initial states       |
  
- Subelements for partial data extraction of any block, decay table, or cross section table in the from {*elem*, *id*, *args*}, where *args* is a sequence of arguments that is passed to the data object(s) extracted by the specification {*elem*, *id*}.

### `LesHouchesDataObject`

`LesHouchesDataObject` is a container for block, decay table, and cross section table data of the form

```mathematica
LesHouchesDataObject["Block" | "Decay" | "XSection", <|...|>]
```

where the ellipsis represents the stored data. 

Each data object has static attibutes called properties. A list of the names of available properties is given by

```mathematica
LesHouchesDataObject[...]["Properties"]
```

and individual property values can be determined by passing the property name to the data object.

Each data object additionally has a set of associated functions called methods which allow extraction and transformation of the stored data. A list of the names of available methods is given by

```mathematica
LesHouchesDataObject[...]["Methods"]
```

Individual methods can be evaluated by passing their names to the data object. Some methods may take additional optional arguments that may be passed along with the method name.

#### Block structure

- Properties:

  | property name           | value form               |
  | ----------------------- | ------------------------ |
  | "Structure"             | "Block"                  |
  | "Name"                  | block name string        |
  | "Renormalization Scale" | number                   |
  | "Description"           | block description string |

- Methods:

  | method name               | result form                             |
  | ------------------------- | --------------------------------------- |
  | "RawData"                 | all block data                          |
  | "Data"                    | block data in a list of associations    |
  | "Dataset"                 | block data in a `Dataset`               |
  | "Table"                   | block data in a two-dimensional array   |
  | "Keys"                    | list of entry keys                      |
  | "Values"                  | list of entry values                    |
  | "Descriptions"            | list of entry descriptions              |
  | "Rules"                   | list of `key -> value` rules            |
  | "Matrix"                  | matrix of values, if available          |
  | "SparseArray"             | `SparseArray` of values, indexed by key |
  | "HeaderPrintFormatString" | block header line Fortran format string |
  | "DataPrintFormatString"   | block data lines Fortran format string  |
  | "Plaintext"               | plain text representation of the block  |

  + Method arguments:
    
    | method name    | optional argument sequence                          |
    | -------------- | --------------------------------------------------- |
    | "Data"         | function to apply to resulting list of associations |
    | "Dataset"      | passed to resulting `Dataset`                       |
    | "Table"        | passed to `Part` operating on resulting array       |
    | "Keys"         | passed to `Part` operating on resulting list        |
    | "Values"       | passed to `Part` operating on resulting list        |
    | "Descriptions" | passed to `Part` operating on resulting list        |
    | "Rules"        | passed to `Lookup` operating on resulting list      |
    | "Matrix"       | passed to `Part` operating on resulting array       |
    | "SparseArray"  | passed to `Part` operating on resulting array       |

In addition, some syntactic sugar has been added to allow various shortcuts for the block structure:

1. Values can be extracted from blocks by passing a key directly to the data object .

2. Values can be changed by passing a `key -> value` rule (or list of rules) directly to the data object.

3. Entries or properties can be appropriately added using the builtin `Append` and `Prepend` functions.

#### Decay table structure

- Properties:

  | property name | value form                     |
  | ------------- | ------------------------------ |
  | "Structure"   | "Decay"                        |
  | "InitalState" | PDG number                     |
  | "TotalWidth"  | number                         |
  | "Description" | decay table description string |
  
- Methods:

  | method name               | result form                                   |
  | ------------------------- | --------------------------------------------- |
  | "RawData"                 | all decay data                                |
  | "Data"                    | decay data in a list of associations          |
  | "Dataset"                 | decay data in a `Dataset`                     |
  | "Table"                   | decay data in a two-dimensional array         |
  | "FinalStates"             | list of lists of final state PDG numbers      |
  | "BranchingRatios"         | list of branching ratios                      |
  | "Descriptions"            | list of entry descriptions                    |
  | "Rules"                   | list of `finalStates -> branchingRatio` rules |
  | "HeaderPrintFormatString" | decay header line Fortran format string       |
  | "DataPrintFormatString"   | decay data lines Fortran format string        |
  | "Plaintext"               | plain text representation of the block        |
  
  + Method arguments:
  
    | method name       | optional argument sequence                          |
    | ----------------- | --------------------------------------------------- |
    | "Data"            | function to apply to resulting list of associations |
    | "Dataset"         | passed to resulting `Dataset`                       |
    | "Table"           | passed to `Part` operating on resulting array       |
    | "BranchingRatios" | passed to `Part` operating on resulting list        |
    | "Descriptions"    | passed to `Part` operating on resulting list        |
    | "Rules"           | passed to `Lookup` operating on resulting list      |

#### Cross section table structure

- Properties:

  | property name        | value form                                           |
  | -------------------- | ---------------------------------------------------- |
  | "Structure"          | "XSection"                                           |
  | "CenterOfMassEnergy" | number                                               |
  | "InitialStates"      | list of PDG numbers                                  |
  | "FinalStates"        | list of PDG numbers                                  |
  | "ProcessRule"        | rule between initial state list and final state list |
  | "Description"        | cross section table description string               |
  
- Methods:

  | method name               | result form                                     |
  | ------------------------- | ----------------------------------------------- |
  | "RawData"                 | all cross section data                          |
  | "Data"                    | cross section data in a list of associations    |
  | "Dataset"                 | cross section data in a `Dataset`               |
  | "Table"                   | cross section data in a two-dimensional array   |
  | "HeaderPrintFormatString" | cross section header line Fortran format string |
  | "DataPrintFormatString"   | cross section data lines Fortran format string  |
  | "Plaintext"               | plain text representation of the block          |
  
  + Method arguments:
  
    | method name | optional argument sequence                          |
    | ----------- | --------------------------------------------------- |
    | "Data"      | function to apply to resulting list of associations |
    | "Dataset"   | passed to resulting `Dataset`                       |
    | "Table"     | passed to `Part` operating on resulting array       |

## TODO

- Build an export function
- Introduce support for the beyond-the-standard-model Les Houches event file format ([arXiv:0712.3311](https://arxiv.org/abs/0712.3311)), which essentially means allowing QNUMBER blocks and possibly adding an import/export format alias.

## Project Information

### Licensing

This project is released under the MIT license.

### Contributions

This package is maintained by Andrew Miller (and primarily created for my own needs). Pull requests and suggestions are always welcomed.