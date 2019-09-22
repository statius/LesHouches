(* ::Package:: *)

(* ::Title:: *)
(*LesHouches*)


(* ::Subsubsection:: *)
(*metadata*)


(* :Title: LesHouches *)
(* :Author: Andrew Miller <amiller@physics.umn.edu> *)
(* :Context: LesHouches` *)
(* :Version: 0.1.0 *)
(* :Date: 2016-11-01 *)
(* :Mathematica Version: 10.2 *)
(* :Copyright: (c) 2016 Andrew Miller *)


(* ::Title::GrayLevel[0]:: *)
(*context LesHouches`*)


(* ::Subsubsection::GrayLevel[0]::Closed:: *)
(*begin package context*)


BeginPackage @ "LesHouches`";


(* ::Section:: *)
(*package information*)


(* ::Subsubsection::Closed:: *)
(*general*)


LesHouches`Developer`$Version = "0.1.0 (November 1, 2016)";


LesHouches`Developer`$VersionNumber = StringReplace[LesHouches`Developer`$Version, "." ~~ Except["."] .. ~~ EndOfString :> ""];


LesHouches`Developer`$ReleaseNumber = StringSplit[LesHouches`Developer`$Version, {" ", "."}][[3]];


LesHouches`Developer`$CreationDate := DateObject @ Last @ StringSplit[LesHouches`Developer`$Version, {"(", ")"}]


LesHouches`Developer`$PackageDirectory = DirectoryName @ $InputFileName;


(* ::Section:: *)
(*usage messages*)


(* ::Subsection:: *)
(*(un)installation*)


(* ::Subsubsection::Closed:: *)
(*InstallLesHouches*)


InstallLesHouches::usage = "InstallLesHouches[\"component\"] installs the components associated with the LesHouches package to the $UserBaseDirectory file tree.
InstallLesHouches[\"Components\"] gives a list of the installable components.";


(* ::Subsubsection::Closed:: *)
(*UninstallLesHouches*)


UninstallLesHouches::usage = "UninstallLesHouches[\"component\"] uninstalls the components associated with the LesHouches package from the $UserBaseDirectory file tree.";


(* ::Subsection:: *)
(*data objects*)


(* ::Subsubsection::Closed:: *)
(*LesHouchesDataObject*)


LesHouchesDataObject::usage = "LesHouchesDataObject[\[Ellipsis]] is a container for block, decay table, and cross section table data."


(* ::Chapter:: *)
(*context LesHouches`Private`*)


(* ::Subsubsection::GrayLevel[0]::Closed:: *)
(*begin private context*)


Begin @ "`Private`";


(* ::Section:: *)
(*utilities*)


(* ::Subsection:: *)
(*string parsers*)


(* ::Subsubsection::Closed:: *)
(*string output stream method*)


If[
  ! MemberQ[$OutputStreamMethods, "String"],

  DefineOutputStreamMethod[
    "String",
    {
     "ConstructorFunction" -> Function[
       {streamname, isAppend, caller, opts},
       
       With[
         {state = Unique @ "StringOutputStream"},
         
         state @ "string" ="";
         state @ "pos" = 0;
         
         {True,state}
         
       ]
     ],
     
     "CloseFunction" -> Function[state, ClearAll @ state],
     
     "StreamPositionFunction" -> Function[state, {state @ "pos", state}],
     
     "WriteFunction" -> Function[
       {state, bytes},
       
       state @ "pos" += Length @ bytes;
       state @ "string" = StringJoin[state @ "string", FromCharacterCode @ bytes];
       
       {Length @ bytes, state}
     ],
  
     "OptionChangesFunction" -> Function[
       {state, opts}, 
       {Append[opts, "String" -> state @ "string"], state}
     ]
    }
  ]

]


(* ::Subsubsection::Closed:: *)
(*stringLineJoin*)


stringLineJoin[s : {__String}] := Module[
  {
   stream = OpenWrite["string", FormatType -> OutputForm, Method -> "String"]
  },
  
  Scan[WriteLine[stream, #] &, s]; 
  
  With[
   {
    string = "String" /. Rest[Method /. Options @ stream]
   },
   
   Close @ stream;
   string
   
  ]
]


(* ::Section:: *)
(*(un)installation*)


(* ::Subsection:: *)
(*installation*)


(* ::Subsubsection:: *)
(*iInstallLesHouches*)


(* ::Subsubsubsection::Closed:: *)
(*info*)


Options @ iInstallLesHouches = Options @ CopyFile;


(* ::Subsubsubsection::Closed:: *)
(*format mappings*)


iInstallLesHouches["FormatMappings", opts : OptionsPattern[]] := With[
  {
   $aliasTargetFile = FileNameJoin[
     {
      $UserBaseDirectory, 
      "SystemFiles", 
      "Formats", 
      "FormatMappings.m"
     }
   ],
   $aliasSourceFile = FileNameJoin[
     {
      LesHouches`Developer`$PackageDirectory, 
      "Formats", 
      "FormatMappings.m"
     }
   ]
  },

  If[
    ! FileExistsQ @ $aliasTargetFile,

    If[
      ! DirectoryQ[DirectoryName @ $aliasTargetFile],
      
      Echo[
        "Directory " ~~ DirectoryName @ $aliasTargetFile ~~ " created.", 
        "InstallLesHouches: ",
        Style[#, "SR", FontColor -> Gray] &
      ];
      CreateDirectory[Directory @ $aliasTargetFile]
    ];
    
    CopyFile[
      $aliasSourceFile, 
      $aliasTargetFile, 
      Evaluate@FilterRules[{opts}, Options@CopyFile]
    ];
     
    Echo[
      "Copied " ~~ $aliasSourceFile ~~ " to " ~~ $aliasTargetFile ~~ ".", 
      "InstallLesHouches: ",
      Style[#, "SR", FontColor -> Gray] &
    ],
     
    Export[
      $aliasTargetFile,
      
      StringJoin[
        StringReplace[
          ToString[
            Union[Import[$aliasTargetFile, "Package"], Import[$aliasSourceFile, "Package"]],
            InputForm
          ],
          {"," -> ",\n", "{" -> "{\n ", "}" -> "\n}"}
        ]
      ],
      
      "Text"
    ];
     
    Echo[
      "Added LesHouches format aliases to " ~~ $aliasTargetFile ~~ ".", 
      "InstallLesHouches: ",
      Style[#, "SR", FontColor -> Gray] &
    ]
  ]

]


(* ::Subsubsubsection::Closed:: *)
(*converters*)


iInstallLesHouches["Converters", opts : OptionsPattern[]] := With[
  {
   $formatTargetDirectory = FileNameJoin[
     {
      $UserBaseDirectory, 
      "SystemFiles", 
      "Formats", 
      "LesHouches"
     }
   ],
   $formatSourceDirectory = FileNameJoin[
     {
      LesHouches`Developer`$PackageDirectory, 
      "Formats", 
      "LesHouches"
     }
   ]
  },
  
  If[
    !DirectoryQ @ $formatTargetDirectory,
    
    Echo[
      "Directory " ~~ $formatTargetDirectory ~~ " created.", 
      "InstallLesHouches: ",
      Style[#, "SR", FontColor -> Gray] &
    ];
    CreateDirectory @ $formatTargetDirectory
  ];
    
  Export[
    FileNameJoin @ {$formatTargetDirectory, "Import.m"},
    
    StringReplace[
      Import[FileNameJoin @ {$formatSourceDirectory, "Import.template"}, "Text"],
      "SourcesPath" -> FileNameJoin @ {LesHouches`Developer`$PackageDirectory, "Kernel", "init.m"}
    ],
    "Text"
  ];
  
  Echo[
    "Exported LesHouches import rules to " ~~ FileNameJoin @ {$formatTargetDirectory, "Import.m"} ~~ ".", 
    "InstallLesHouches: ",
    Style[#, "SR", FontColor -> Gray] &
  ];
  
  iInstallLesHouches["FormatMappings", opts];
  
]


(* ::Subsubsection::Closed:: *)
(*InstallLesHouches*)


Options @ InstallLesHouches = Options @ iInstallLesHouches;


SyntaxInformation @ InstallLesHouches = {
  "ArgumentsPattern" -> {_, OptionsPattern[]}, 
  "OptionNames" -> Keys @ Options @ InstallLesHouches
};


InstallLesHouches["Components"] = {"Converters"};


InstallLesHouches[
  comp_String /; MemberQ[InstallLesHouches @ "Components", comp], 
  opts : OptionsPattern[]
] := iInstallLesHouches[comp, opts];


(* ::Subsection:: *)
(*uninstallation*)


(* ::Subsubsection:: *)
(*iUninstallLesHouches*)


(* ::Subsubsubsection::Closed:: *)
(*format mappings*)


iUninstallLesHouches["FormatMappings"] := With[
  {
   $aliasTargetFile = FileNameJoin[
     {
      $UserBaseDirectory, 
      "SystemFiles", 
      "Formats", 
      "FormatMappings.m"
     }
   ],
   $aliasSourceFile = FileNameJoin[
     {
      LesHouches`Developer`$PackageDirectory, 
      "Formats", 
      "FormatMappings.m"
     }
   ]
  },

  If[
    ! FileExistsQ @ $aliasTargetFile,
     
    With[
      {
       formats = Complement[Import[$aliasTargetFile, "Package"], Import[$aliasSourceFile, "Package"]]
      },
      
      If[
        Length @ formats > 0,
        
        Export[
          $aliasTargetFile,
          
          StringJoin[
            StringReplace[
              ToString[formats, InputForm],
              {"," -> ",\n", "{" -> "{\n ", "}" -> "\n}"}
            ]
          ],
          
          "Text"
        ],
         
        DeleteFile @ $aliasTargetFile
      ]
     
    ];
    
    Echo[
      "Removed LesHouches format aliases from " ~~ $aliasTargetFile ~~ ".", 
      "UninstallLesHouches: ",
      Style[#, "SR", FontColor -> Gray] &
    ]
  ]

]


(* ::Subsubsubsection::Closed:: *)
(*converters*)


iUninstallLesHouches["Converters"] := With[
  {
   $formatTargetDirectory = FileNameJoin[
     {
      $UserBaseDirectory, 
      "SystemFiles", 
      "Formats", 
      "LesHouches"
     }
   ],
   $formatSourceDirectory = FileNameJoin[
     {
      LesHouches`Developer`$PackageDirectory, 
      "Formats", 
      "LesHouches"
     }
   ]
  },
  
  DeleteDirectory[$formatTargetDirectory, DeleteContents -> True];
  Echo[
    "Removed LesHouches format directory " ~~ $formatTargetDirectory, 
    "UninstallLesHouches: ",
    Style[#, "SR", FontColor -> Gray] &
  ];
  
  iUninstallLesHouches @ "FormatMappings";
  
]


(* ::Subsubsection::Closed:: *)
(*UninstallLesHouches*)


SyntaxInformation @ UninstallLesHouches = {"ArgumentsPattern" -> {_, OptionsPattern[]}};


UninstallLesHouches["Components"] = {"Converters"};


UninstallLesHouches[
  comp_String /; MemberQ[InstallLesHouches @ "Components", comp]
] := iUninstallLesHouches[comp];


(* ::Section:: *)
(*data objects*)


(* ::Subsection:: *)
(*data object properties and methods*)


(* ::Subsubsection::Closed:: *)
(*block*)


$dataObjectProperties @ "Block" = {
  "Name",
  "RenormalizationScale",
  "Description"
};


$dataObjectMethods @ "Block" = {
  "RawData",
  "Data",
  "Dataset",
  "Table",
  "Keys",
  "Values",
  "Descriptions",
  "Rules",
  "Matrix",
  "SparseArray",
  "HeaderPrintFormatString",
  "DataPrintFormatString",
  "Plaintext"
};


(* ::Subsubsection::Closed:: *)
(*decay*)


$dataObjectProperties @ "Decay" = {
  "InitialState",
  "TotalWidth",
  "Description"
};


$dataObjectMethods @ "Decay" = {
  "RawData",
  "Data",
  "Dataset",
  "Table",
  "FinalStates",
  "BranchingRatios",
  "Descriptions",
  "Rules",
  "HeaderPrintFormatString",
  "DataPrintFormatString",
  "Plaintext"
};


(* ::Subsubsection::Closed:: *)
(*xsection*)


$dataObjectProperties @ "XSection" = {
  "CenterOfMassEnergy",
  "InitialStates",
  "FinalStates",
  "ProcessRule",
  "Description"
};


$dataObjectMethods @ "XSection" = {
  "RawData",
  "Data",
  "Dataset",
  "Table",
  "HeaderPrintFormatString",
  "DataPrintFormatString",
  "Plaintext"
};


(* ::Subsection:: *)
(*data object type tests*)


(* ::Subsubsection::Closed:: *)
(*blockDataQ*)


blockDataQ = And[
  AssociationQ @ #, 
  ContainsOnly[Keys @ #, Join[$dataObjectProperties @ "Block", {"Data"}]],
  KeyExistsQ[#, "Name"],
  If[
    KeyExistsQ[#, "Data"],
    
    And[
      MatchQ[# @ "Data", {__Association}],
      AllTrue[
        Keys @ Merge[# @ "Data", Identity], 
        StringMatchQ["Key" | "Value" | "Description"]
      ]
    ],
    
    True
  ]
] &


(* ::Subsubsection::Closed:: *)
(*decayDataQ*)


decayDataQ = And[
  AssociationQ @ #, 
  ContainsOnly[Keys @ #, Join[$dataObjectProperties @ "Decay", {"Data"}]],
  KeyExistsQ[#, "InitialState"],
  KeyExistsQ[#, "TotalWidth"],
  If[
    KeyExistsQ[#, "Data"],
    
    And[
      MatchQ[# @ "Data", {__Association}],
      AllTrue[
        Keys @ Merge[# @ "Data", Identity], 
        StringMatchQ["BranchingRatio" | "FinalStates" | "Description"]
      ]
    ],
    
    True
  ]
] &


(* ::Subsubsection::Closed:: *)
(*xsectionDataQ*)


xsectionDataQ = And[
  AssociationQ @ #, 
  ContainsOnly[Keys @ #, Join[$dataObjectProperties @ "XSection", {"Data"}]],
  KeyExistsQ[#, "ProcessRule"],
  KeyExistsQ[#, "CenterOfMassEnergy"],
  If[
    KeyExistsQ[#, "Data"],
    
    And[
      MatchQ[# @ "Data", {__Association}],
      AllTrue[
        Keys @ Merge[# @ "Data", Identity], 
        Alternatives[
          "ScaleScheme", 
          "QCDOrder", 
          "EWOrder", 
          "KappaFactorization", 
          "KappaRenormalization", 
          "PartonDistributionFunctionID", 
          "CrossSection", 
          "ProgramName", 
          "ProgramVersion", 
          "Description"
        ] // StringMatchQ
      ]
    ],
    
    True
  ]
] &


(* ::Subsection:: *)
(*iLesHouchesDataObject*)


(* ::Subsubsubsection::Closed:: *)
(*info*)


Options @ iLesHouchesDataObject = {};


(* ::Subsubsection:: *)
(*properties*)


(* ::Subsubsubsection::Closed:: *)
(*"Properties"*)


iLesHouchesDataObject[
  structure : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["Properties"] := Intersection[Keys @ assoc, Join[{"Structure"}, $dataObjectProperties @ structure]]


(* ::Subsubsubsection::Closed:: *)
(*"Structure"*)


iLesHouchesDataObject[
  structure : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["Structure"] := structure


(* ::Subsubsubsection::Closed:: *)
(*all other properties*)


iLesHouchesDataObject[
  structure : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
][prop_String] := Lookup[assoc, prop, Missing["NotAvailable", prop]]


(* ::Subsubsection:: *)
(*methods*)


(* ::Subsubsubsection::Closed:: *)
(*"Methods"*)


iLesHouchesDataObject[
  structure : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["Methods"] := $dataObjectMethods @ structure


(* ::Subsubsubsection::Closed:: *)
(*"RawData"*)


iLesHouchesDataObject[
  structure : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["RawData"] := assoc


(* ::Subsubsubsection::Closed:: *)
(*"Data"*)


iLesHouchesDataObject[
  structure : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["Data"] := Lookup[assoc, "Data", Missing["NotAvailable", "Data"]]


iLesHouchesDataObject[
  structure : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["Data", f_] := With[
  {
   data = iLesHouchesDataObject[structure, assoc, opts]["Data"]
  },
  
  If[
    ! MissingQ @ data,
    f @ data,
    data
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"Dataset"*)


iLesHouchesDataObject[
  structure : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["Dataset"] := With[
  {
   data = iLesHouchesDataObject[structure, assoc, opts]["Data"]
  },
  
  If[
    ! MissingQ @ data,
    Dataset @ data,
    Missing["NotAvailable", "Dataset"]
  ]
  
]


iLesHouchesDataObject[
  structure : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["Dataset", args__] := With[
  {
   dataset = iLesHouchesDataObject[structure, assoc, opts]["Dataset"]
  },
  
  If[
    ! MissingQ @ dataset,
    dataset @ args,
    dataset
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"Table"*)


iLesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Table"] := With[
  {
   data = iLesHouchesDataObject["Block", assoc, opts]["Data"]
  },
  
  If[
    ! MissingQ @ data,
    Lookup[data, {"Key", "Value", "Description"}, Missing @ "NotAvailable"],
    Missing["NotAvailable", "Table"]
  ]
  
]


iLesHouchesDataObject[
  structure : ("Decay" | "XSection"), 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Table"] := With[
  {
   data = iLesHouchesDataObject[structure, assoc, opts]["Data"]
  },
  
  If[
    ! MissingQ @ data,
    Lookup[data, Keys @ Merge[data, Identity], Missing @ "NotAvailable"],
    Missing["NotAvailable", "Table"]
  ]
  
]


iLesHouchesDataObject[
  structure : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["Table", args : Repeated[_, {1, 2}]] := With[
  {
   table = iLesHouchesDataObject[structure, assoc, opts]["Table"]
  },
  
  If[
    ! MissingQ @ table,
    table[[args]],
    table
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"Keys"*)


iLesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Keys"] := With[
  {
   data = iLesHouchesDataObject["Block", assoc, opts]["Data"]
  },
  
  If[
    ! MissingQ @ data,
    Lookup[data, "Key", Nothing],
    Missing["NotAvailable", "Keys"]
  ]
  
]


iLesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Keys", args__] := With[
  {
   keys = iLesHouchesDataObject["Block", assoc, opts]["Keys"]
  },
  
  If[
    ! MissingQ @ keys,
    keys[[args]],
    keys
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"Values"*)


iLesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Values"] := With[
  {
   data = iLesHouchesDataObject["Block", assoc, opts]["Data"]
  },
  
  If[
    ! MissingQ @ data,
    Lookup[data, "Value", Nothing],
    Missing["NotAvailable", "Values"]
  ]
  
]


iLesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Values", args__] := With[
  {
   values = iLesHouchesDataObject["Block", assoc, opts]["Values"]
  },
  
  If[
    ! MissingQ @ values,
    values[[args]],
    values
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"FinalStates"*)


iLesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["FinalStates"] := With[
  {
   data = iLesHouchesDataObject["Decay", assoc, opts]["Data"]
  },
  
  If[
    ! MissingQ @ data,
    Lookup[data, "FinalStates", Nothing],
    Missing["NotAvailable", "FinalStates"]
  ]
  
]


iLesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["FinalStates", args__] := With[
  {
   states = iLesHouchesDataObject["Decay", assoc, opts]["FinalStates"]
  },
  
  If[
    ! MissingQ @ states,
    states[[args]],
    states
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"BranchingRatios"*)


iLesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["BranchingRatios"] := With[
  {
   data = iLesHouchesDataObject["Decay", assoc, opts]["Data"]
  },
  
  If[
    ! MissingQ @ data,
    Lookup[data, "BranchingRatios", Nothing],
    Missing["NotAvailable", "BranchingRatios"]
  ]
  
]


iLesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["BranchingRatios", args__] := With[
  {
   bratios = iLesHouchesDataObject["Decay", assoc, opts]["BranchingRatios"]
  },
  
  If[
    ! MissingQ @ bratios,
    bratios[[args]],
    bratios
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"Descriptions"*)


iLesHouchesDataObject[
  spec : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["Values"] := With[
  {
   data = iLesHouchesDataObject[spec, assoc, opts]["Data"]
  },
  
  If[
    ! MissingQ @ data,
    Lookup[data, "Descriptions", Nothing],
    Missing["NotAvailable", "Descriptions"]
  ]
  
]


iLesHouchesDataObject[
  spec : ("Block" | "Decay" | "XSection"), 
  assoc_, 
  opts : OptionsPattern[]
]["Values", args__] := With[
  {
   descr = iLesHouchesDataObject[spec, assoc, opts]["Descriptions"]
  },
  
  If[
    ! MissingQ @ descr,
    descr[[args]],
    descr
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"Rules"*)


iLesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Rules"] := With[
  {
   table = iLesHouchesDataObject["Block", assoc, opts]["Table"]
  },
  
  If[
    ! MissingQ @ table,
    Rule @@@ table[[All, {1, 2}]] /. _Missing -> _,
    Missing["NotAvailable", "Rules"]
  ]
  
]


iLesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Rules"] := With[
  {
   table = iLesHouchesDataObject["Decay", assoc, opts]["Table"]
  },
  
  If[
    ! MissingQ @ table,
    Rule @@@ table[[All, {2, 1}]] /. _Missing -> _,
    Missing["NotAvailable", "Rules"]
  ]
  
]


iLesHouchesDataObject[
  spec : ("Block" | "Decay"), 
  assoc_, 
  opts : OptionsPattern[]
]["Rules", args : Repeated[_, {1, 2}]] := With[
  {
   rules = iLesHouchesDataObject[spec, assoc, opts]["Rules"]
  },
  
  If[
    ! MissingQ @ rules,
    Lookup[rules, args],
    rules
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"SparseArray"*)


iLesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["SparseArray"] := With[
  {
   rules = iLesHouchesDataObject["Block", assoc, opts]["Rules"]
  },
  
  If[
    ! MissingQ @ rules,
    SparseArray @ rules,
    Missing["NotAvailable", "SparseArray"]
  ]
  
]


iLesHouchesDataObject[
  "Block", 
  assoc_, 
  opts : OptionsPattern[]
]["SparseArray", args : Repeated[_, {1, 2}]] := With[
  {
   array = iLesHouchesDataObject["Block", assoc, opts]["SparseArray"]
  },
  
  If[
    ! MissingQ @ array,
    array[[args]],
    array
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"Matrix"*)


iLesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Matrix"] := With[
  {
   array = iLesHouchesDataObject["Block", assoc, opts]["SparseArray"]
  },
  
  If[
    And[! MissingQ @ array, MatrixQ[Normal @ array]],
    Normal @ array,
    Missing["NotAvailable", "Matrix"]
  ]
  
]


iLesHouchesDataObject[
  "Block", 
  assoc_, 
  opts : OptionsPattern[]
]["Matrix", args : Repeated[_, {1, 2}]] := With[
  {
   matrix = iLesHouchesDataObject["Block", assoc, opts]["Matrix"]
  },
  
  If[
    ! MissingQ @ matrix,
    matrix[[args]],
    matrix
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"HeaderPrintFormatString"*)


iLesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["HeaderPrintFormatString"] := StringTemplate["('BLOCK',1x,A``)"][
  If[
    KeyExistsQ[assoc, "RenormalizationScale"], 
    
    If[
      KeyExistsQ[assoc, "Description"],
      ",1x,'Q ='1x,1P,E16.8,0P,3x,'#',1x,A",
      ",1x,'Q ='1x,1P,E16.8,0P"
    ],
    
    ""
  ]
]


iLesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["HeaderPrintFormatString"] := StringTemplate["('DECAY',1x,I9,3x,1P,E16.8,0P``)"][
  If[
    KeyExistsQ[assoc, "Description"], 
    ",3x,'#',1x,A",
    ""
  ]
]


iLesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["HeaderPrintFormatString"] := StringTemplate["('XSECTION',1x,1P,E16.8,0P,3x,2(I9,1x),I2,``)"][
  StringJoin[
    IntegerString @ Length[assoc @ "FinalStates"] <> "(1x,I9)",
    If[
      KeyExistsQ[assoc, "Description"], 
      ",3x,'#',1x,A",
      ""
    ]
  ]
]


(* ::Subsubsubsection::Closed:: *)
(*"DataPrintFormatString"*)


iLesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["DataPrintFormatString"] := With[
  {
   data = Merge[Lookup[assoc, "Data", {}], Identity]
  },
  
  If[
    Length @ data > 0, 
    
    StringTemplate["(``,``)"][
      If[
        KeyExistsQ[data, "Key"], 
        
        If[
          MatrixQ[data @ "Key"],
          IntegerString @ Length[First @ data @ "Key"] <> "(1x,I2)" <> ",3x",
          "1x,I9,3x"
        ], 
        
        "9x"
      ],
      
      If[
        VectorQ[data @ "Value", StringQ],
        
        "A",
        
        If[
          KeyExistsQ[data, "Description"],
          "1P,E16.8,0P,3x,'#',1x,A",
          "1P,E16.8,0P"
        ]
      ]
    ],
    
    None
  ]
  
]


iLesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["DataPrintFormatString"] := With[
  {
   data = Merge[Lookup[assoc, "Data", {}], Identity]
  },
  
  If[
    Length @ data > 0, 
    
    StringTemplate["(3x,1P,E16.8,0P,3x,I2,2X,``)"][
      StringJoin[
        IntegerString @ Length[First @ data @ "FinalStates"] <> "(1x,I9)",
        If[
          KeyExistsQ[data, "Description"], 
          ",3x,'#',1x,A",
          ""
        ]
      ]
    ],
    
    None
  ]
  
]


iLesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ,  
  opts : OptionsPattern[]
]["DataPrintFormatString"] := With[
  {
   data = Merge[Lookup[assoc, "Data", {}], Identity]
  },
  
  If[
    Length @ data > 0, 
    
    StringTemplate["(3(1x,I2),1P,2(3x,E16.8),0P,3x,I5,3x,1P,E16.8,0P,2(3x,A)``)"][
      If[
        KeyExistsQ[data, "Description"], 
        ",3x,'#',1x,A",
        ""
      ]
    ],
    
    Automatic
  ]
  
]


(* ::Subsection:: *)
(*LesHouchesDataObject*)


(* ::Subsubsubsection::Closed:: *)
(*info*)


Options @ LesHouchesDataObject = Options @ iLesHouchesDataObject;


SyntaxInformation @ LesHouchesDataObject = {
  "ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
  "OptionNames" -> Keys @ Options @ LesHouchesDataObject
};


(* ::Subsubsection:: *)
(*block*)


(* ::Subsubsubsection:: *)
(*constructors*)


(* ::Subsubsubsection:: *)
(*behaviors*)


(* ::Subsubsubsubsection:: *)
(*extract values by key*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
][key : (_Integer | {__Integer})] := With[
  {
   data = LesHouchesDataObject["Block", assoc, opts]["Rules"]
  },
  
  If[
    ! MissingQ @ data,
    Merge[Association /@ data, Identity][key] /. {x_} :> x,
    data
  ]
]


(* ::Subsubsubsubsection::Closed:: *)
(*extract all data*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
][All] := LesHouchesDataObject["Block", assoc, opts]["RawData"]


(* ::Subsubsubsubsection::Closed:: *)
(*extract keys*)


LesHouchesDataObject /: Keys[
  LesHouchesDataObject[
    "Block", 
    assoc_? blockDataQ, 
    opts : OptionsPattern[]
  ]
] := LesHouchesDataObject["Block", assoc, opts]["Keys"]


(* ::Subsubsubsubsection::Closed:: *)
(*extract values*)


LesHouchesDataObject /: Values[
  LesHouchesDataObject[
    "Block", 
    assoc_? blockDataQ, 
    opts : OptionsPattern[]
  ]
] := LesHouchesDataObject["Block", assoc, opts]["Values"]


(* ::Subsubsubsubsection::Closed:: *)
(*change values by key*)


(
 obj : LesHouchesDataObject[
   "Block", 
   assoc_? blockDataQ, 
   opts : OptionsPattern[]
 ]
)[rule : Rule[key_, (_? NumericQ | _String)]] := LesHouchesDataObject[
  "Block", 
  assoc, 
  opts
][{rule}] /; MemberQ[Join[Keys @ assoc, obj @ "Keys" /. _Missing -> {}], key]


(
 obj : LesHouchesDataObject[
   "Block", 
   assoc_? blockDataQ, 
   opts : OptionsPattern[]
 ]
)[rules : {Rule[_, (_? NumericQ | _String)] ..}] := ReplacePart[
  obj, 
  (Position[obj, KeyValuePattern[#1 -> _]][[1]] -> #2) & @@@ rules
] /; ContainsOnly[Keys @ rules, Keys @ assoc]


(
 obj : LesHouchesDataObject[
   "Block", 
   assoc_? blockDataQ, 
   opts : OptionsPattern[]
 ]
)[rules : {Rule[_, (_? NumericQ | _String)] ..}] := ReplacePart[
  obj, 
  (Join[Position[obj, KeyValuePattern["Key" -> #1], \[Infinity]][[1]], {2}] -> #2) & @@@ rules
] /; ContainsOnly[Keys @ rules, obj @ "Keys" /. _Missing -> {}]


(* ::Subsubsubsubsection::Closed:: *)
(*append and prepend*)


LesHouchesDataObject /: (f : (Append | Prepend))[
  obj : LesHouchesDataObject[
    "Block", 
    assoc_? blockDataQ, 
    opts : OptionsPattern[]
  ],
  rule : Rule[key : ("Key" | "Value" | "Description" | "Data"), _]
] := LesHouchesDataObject["Block", f[assoc, rule], opts]


LesHouchesDataObject /: (f : (Append | Prepend))[
  obj : LesHouchesDataObject[
    "Block", 
    assoc_? blockDataQ, 
    opts : OptionsPattern[]
  ],
  rules : {Rule[("Key" | "Value" | "Description" | "Data"), _] ..}
] := LesHouchesDataObject["Block", f[assoc, rules], opts]


LesHouchesDataObject /: (f : (Append | Prepend))[
  obj : LesHouchesDataObject[
    "Block", 
    assoc_? blockDataQ, 
    opts : OptionsPattern[]
  ],
  elem_Association /; ContainsOnly[Keys @ elem, {"Key", "Value", "Description"}]
] := LesHouchesDataObject[
  "Block", 
  <|assoc, "Data" -> f[Lookup[assoc, "Data", <||>], elem]|>, 
  opts
]


LesHouchesDataObject /: (f : (Append | Prepend))[
  obj : LesHouchesDataObject[
    "Block", 
    assoc_? blockDataQ, 
    opts : OptionsPattern[]
  ],
  elem : {__Association} /; ContainsOnly[Union @ Flatten[Keys /@ elem], {"Key", "Value", "Description"}]
] := LesHouchesDataObject[
  "Block", 
  <|assoc, "Data" -> f[Lookup[assoc, "Data", <||>], elem]|>, 
  opts
]


(* ::Subsubsubsection:: *)
(*properties*)


(* ::Subsubsubsubsection::Closed:: *)
(*"Properties"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Properties"] := iLesHouchesDataObject["Block", assoc, opts]["Properties"]


(* ::Subsubsubsubsection::Closed:: *)
(*"Structure"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Structure"] := iLesHouchesDataObject["Block", assoc, opts]["Structure"]


(* ::Subsubsubsubsection::Closed:: *)
(*all other properties*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
][prop_String /; MemberQ[$dataObjectProperties @ "Block", prop]] := iLesHouchesDataObject["Block", assoc, opts][prop]


(* ::Subsubsubsubsection::Closed:: *)
(*"Q"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Q"] := Lookup[assoc, "RenormalizationScale", Missing["NotAvailable", "Q"]]


(* ::Subsubsubsection:: *)
(*methods*)


(* ::Subsubsubsubsection::Closed:: *)
(*"Methods"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Methods"] := iLesHouchesDataObject["Block", assoc, opts]["Methods"]


(* ::Subsubsubsubsection::Closed:: *)
(*"RawData"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["RawData"] := iLesHouchesDataObject["Block", assoc, opts]["RawData"]


(* ::Subsubsubsubsection::Closed:: *)
(*"Data"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Data"] := iLesHouchesDataObject["Block", assoc, opts]["Data"]


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Data", f_] := iLesHouchesDataObject["Block", assoc, opts]["Data", f]


(* ::Subsubsubsubsection::Closed:: *)
(*"Dataset"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Dataset"] := iLesHouchesDataObject["Block", assoc, opts]["Dataset"]


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Dataset", args__] := iLesHouchesDataObject["Block", assoc, opts]["Dataset", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Table"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Table"] := iLesHouchesDataObject["Block", assoc, opts]["Table"]


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Table", args__] := iLesHouchesDataObject["Block", assoc, opts]["Table", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Keys"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Keys"] := iLesHouchesDataObject["Block", assoc, opts]["Keys"]


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Keys", args__] := iLesHouchesDataObject["Block", assoc, opts]["Keys", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Values"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Values"] := Replace[iLesHouchesDataObject["Block", assoc, opts]["Values"], {x_} :> x]


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Values", args__] := iLesHouchesDataObject["Block", assoc, opts]["Values", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Descriptions"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Descriptions"] := Replace[iLesHouchesDataObject["Block", assoc, opts]["Descriptions"], {x_} :> x]


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Descriptions", args__] := iLesHouchesDataObject["Block", assoc, opts]["Descriptions", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Rules"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Rules"] := iLesHouchesDataObject["Block", assoc, opts]["Rules"]


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Rules", args : Repeated[_, {1, 2}]] := iLesHouchesDataObject["Block", assoc, opts]["Rules", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"SparseArray"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["SparseArray"] := iLesHouchesDataObject["Block", assoc, opts]["SparseArray"]


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["SparseArray", args__] := iLesHouchesDataObject["Block", assoc, opts]["SparseArray", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Matrix"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Matrix"] := iLesHouchesDataObject["Block", assoc, opts]["Matrix"]


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["Matrix", args__] := iLesHouchesDataObject["Block", assoc, opts]["Matrix", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"HeaderPrintFormatString"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["HeaderPrintFormatString"] := iLesHouchesDataObject["Block", assoc, opts]["HeaderPrintFormatString"]


(* ::Subsubsubsubsection::Closed:: *)
(*"DataPrintFormatString"*)


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
]["DataPrintFormatString"] := iLesHouchesDataObject["Block", assoc, opts]["DataPrintFormatString"]


(* ::Subsubsubsection::Closed:: *)
(*failure*)


LesHouchesDataObject[
  "Block",
  data_? (! blockDataQ @ # &),
  opts : OptionsPattern[]
] := Failure[
  "InvalidData",
  Association[
    "Message" -> "Spectrum block cannot be constructed from given data.",
    "Data" -> data
  ]
]


LesHouchesDataObject[
  "Block", 
  assoc_? blockDataQ, 
  opts : OptionsPattern[]
][args___] := Failure[
  "UnknownArgument",
  Association[
    "Message" -> "Unrecognized argument sequence for spectrum block LesHouchesDataObject.",
    "Input" -> {args},
    "Target" -> LesHouchesDataObject["Block", assoc, opts]
  ]
]


(* ::Subsubsubsection::Closed:: *)
(*boxes*)


LesHouchesDataObject /: MakeBoxes[
  obj : LesHouchesDataObject["Block", assoc_? blockDataQ, opts : OptionsPattern[]], 
  form : (StandardForm | TraditionalForm)
] := With[
  {
   above = If[
     KeyExistsQ[assoc, "RenormalizationScale"],
     
     {
      {
       BoxForm`SummaryItem @ {"Block ", assoc @ "Name"}, 
       BoxForm`SummaryItem @ {"Q = ", assoc @ "RenormalizationScale"}
      },
      {
       BoxForm`SummaryItem @ {"Description: ", Replace[assoc @ "Description", _Missing -> None]}, 
       SpanFromLeft
      }
     },
     
     {
      BoxForm`SummaryItem @ {"Block ", assoc @ "Name"},
      BoxForm`SummaryItem @ {"Description: ", Replace[assoc @ "Description", _Missing -> None]}
     }
   ],
    
   below = If[
     MissingQ[obj @ "Dataset"],
     {BoxForm`SummaryItem @ {"Data: ", None}},
     {BoxForm`SummaryItem @ {obj @ "Dataset"}}
   ]
  },
  
  BoxForm`ArrangeSummaryBox[
    LesHouchesDataObject,
    obj,
    None,
    above,
    below,
    form,
    "Interpretable" -> Automatic
  ]
  
]


(* ::Subsubsection:: *)
(*decay*)


(* ::Subsubsubsection:: *)
(*constructors*)


(* ::Subsubsubsection:: *)
(*behaviors*)


(* ::Subsubsubsubsection::Closed:: *)
(*extract all data*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
][All] := LesHouchesDataObject["Decay", assoc, opts]["RawData"]


(* ::Subsubsubsection:: *)
(*properties*)


(* ::Subsubsubsubsection::Closed:: *)
(*"Properties"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Properties"] := iLesHouchesDataObject["Decay", assoc, opts]["Properties"]


(* ::Subsubsubsubsection::Closed:: *)
(*"Structure"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Structure"] := iLesHouchesDataObject["Decay", assoc, opts]["Structure"]


(* ::Subsubsubsubsection::Closed:: *)
(*all other properties*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
][prop_String /; MemberQ[$dataObjectProperties @ "Decay", prop]] := iLesHouchesDataObject["Decay", assoc, opts][prop]


(* ::Subsubsubsection:: *)
(*methods*)


(* ::Subsubsubsubsection::Closed:: *)
(*"Methods"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Methods"] := iLesHouchesDataObject["Decay", assoc, opts]["Methods"]


(* ::Subsubsubsubsection::Closed:: *)
(*"RawData"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["RawData"] := iLesHouchesDataObject["Decay", assoc, opts]["RawData"]


(* ::Subsubsubsubsection::Closed:: *)
(*"Data"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Data"] := iLesHouchesDataObject["Decay", assoc, opts]["Data"]


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Data", f_] := iLesHouchesDataObject["Decay", assoc, opts]["Data", f]


(* ::Subsubsubsubsection::Closed:: *)
(*"Dataset"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Dataset"] := iLesHouchesDataObject["Decay", assoc, opts]["Dataset"]


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Dataset", args__] := iLesHouchesDataObject["Decay", assoc, opts]["Dataset", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Table"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Table"] := iLesHouchesDataObject["Decay", assoc, opts]["Table"]


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Table", args__] := iLesHouchesDataObject["Decay", assoc, opts]["Table", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"FinalStates"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["FinalStates"] := iLesHouchesDataObject["Decay", assoc, opts]["FinalStates"]


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["FinalStates", args__] := iLesHouchesDataObject["Decay", assoc, opts]["FinalStates", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"BranchingRatios"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["BranchingRatios"] := iLesHouchesDataObject["Decay", assoc, opts]["BranchingRatios"]


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["BranchingRatios", args__] := iLesHouchesDataObject["Decay", assoc, opts]["BranchingRatios", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Descriptions"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Descriptions"] := Replace[iLesHouchesDataObject["Decay", assoc, opts]["Descriptions"], {x_} :> x]


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Descriptions", args__] := iLesHouchesDataObject["Decay", assoc, opts]["Descriptions", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Rules"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Rules"] := iLesHouchesDataObject["Decay", assoc, opts]["Rules"]


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["Rules", args : Repeated[_, {1, 2}]] := iLesHouchesDataObject["Decay", assoc, opts]["Rules", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"HeaderPrintFormatString"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["HeaderPrintFormatString"] := iLesHouchesDataObject["Decay", assoc, opts]["HeaderPrintFormatString"]


(* ::Subsubsubsubsection::Closed:: *)
(*"DataPrintFormatString"*)


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
]["DataPrintFormatString"] := iLesHouchesDataObject["Decay", assoc, opts]["DataPrintFormatString"]


(* ::Subsubsubsection::Closed:: *)
(*failure*)


LesHouchesDataObject[
  "Decay",
  data_? (! decayDataQ @ # &),
  opts : OptionsPattern[]
] := Failure[
  "InvalidData",
  Association[
    "Message" -> "Decay table cannot be constructed from given data.",
    "Data" -> data
  ]
]


LesHouchesDataObject[
  "Decay", 
  assoc_? decayDataQ, 
  opts : OptionsPattern[]
][args___] := Failure[
  "UnknownArgument",
  Association[
    "Message" -> "Unrecognized argument sequence for decay table LesHouchesDataObject.",
    "Input" -> {args},
    "Target" -> LesHouchesDataObject["Decay", assoc, opts]
  ]
]


(* ::Subsubsubsection::Closed:: *)
(*boxes*)


LesHouchesDataObject /: MakeBoxes[
  obj : LesHouchesDataObject["Decay", assoc_? decayDataQ, opts : OptionsPattern[]], 
  form : (StandardForm | TraditionalForm)
] := With[
  {
   above = {
     {
      BoxForm`SummaryItem @ {"Decay ", assoc @ "InitialState"}, 
      BoxForm`SummaryItem @ {"\[CapitalGamma] = ", assoc @ "TotalWidth"}
     },
     {
      BoxForm`SummaryItem @ {"Description: ", Replace[assoc @ "Description", _Missing -> None]}, 
      SpanFromLeft
     }
   },
    
   below = If[
     MissingQ[obj @ "Dataset"],
     {BoxForm`SummaryItem @ {"Data: ", None}},
     {BoxForm`SummaryItem @ {obj @ "Dataset"}}
   ]
  },
  
  BoxForm`ArrangeSummaryBox[
    LesHouchesDataObject,
    obj,
    None,
    above,
    below,
    form,
    "Interpretable" -> Automatic
  ]
  
]


(* ::Subsubsection:: *)
(*xsection*)


(* ::Subsubsubsection:: *)
(*constructors*)


(* ::Subsubsubsection::Closed:: *)
(*behaviors*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
][All] := LesHouchesDataObject["XSection", assoc, opts]["RawData"]


(* ::Subsubsubsection:: *)
(*properties*)


(* ::Subsubsubsubsection::Closed:: *)
(*"Properties"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Properties"] := iLesHouchesDataObject["XSection", assoc, opts]["Properties"]


(* ::Subsubsubsubsection::Closed:: *)
(*"Structure"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Structure"] := iLesHouchesDataObject["XSection", assoc, opts]["Structure"]


(* ::Subsubsubsubsection::Closed:: *)
(*"InitialStates"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["InitialStates"] := LesHouchesDataObject["XSection", assoc, opts]["ProcessRule"] // First


(* ::Subsubsubsubsection::Closed:: *)
(*"FinalStates"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["InitialStates"] := LesHouchesDataObject["XSection", assoc, opts]["ProcessRule"] // Last


(* ::Subsubsubsubsection::Closed:: *)
(*all other properties*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
][prop_String /; MemberQ[$dataObjectProperties @ "XSection", prop]] := iLesHouchesDataObject["XSection", assoc, opts][prop]


(* ::Subsubsubsection:: *)
(*methods*)


(* ::Subsubsubsubsection::Closed:: *)
(*"Methods"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Methods"] := iLesHouchesDataObject["XSection", assoc, opts]["Methods"]


(* ::Subsubsubsubsection::Closed:: *)
(*"RawData"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["RawData"] := iLesHouchesDataObject["XSection", assoc, opts]["RawData"]


(* ::Subsubsubsubsection::Closed:: *)
(*"Data"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Data"] := iLesHouchesDataObject["XSection", assoc, opts]["Data"]


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Data", f_] := iLesHouchesDataObject["XSection", assoc, opts]["Data", f]


(* ::Subsubsubsubsection::Closed:: *)
(*"Dataset"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Dataset"] := iLesHouchesDataObject["XSection", assoc, opts]["Dataset"]


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Dataset", args__] := iLesHouchesDataObject["XSection", assoc, opts]["Dataset", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Table"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Table"] := iLesHouchesDataObject["XSection", assoc, opts]["Table"]


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Table", args__] := iLesHouchesDataObject["XSection", assoc, opts]["Table", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"Descriptions"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Descriptions"] := Replace[iLesHouchesDataObject["XSection", assoc, opts]["Descriptions"], {x_} :> x]


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["Descriptions", args__] := iLesHouchesDataObject["XSection", assoc, opts]["Descriptions", args]


(* ::Subsubsubsubsection::Closed:: *)
(*"HeaderPrintFormatString"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["HeaderPrintFormatString"] := iLesHouchesDataObject["XSection", assoc, opts]["HeaderPrintFormatString"]


(* ::Subsubsubsubsection::Closed:: *)
(*"DataPrintFormatString"*)


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
]["DataPrintFormatString"] := iLesHouchesDataObject["XSection", assoc, opts]["DataPrintFormatString"]


(* ::Subsubsubsection::Closed:: *)
(*failure*)


LesHouchesDataObject[
  "XSection",
  data_? (! xsectionDataQ @ # &),
  opts : OptionsPattern[]
] := Failure[
  "InvalidData",
  Association[
    "Message" -> "Cross section table cannot be constructed from given data.",
    "Data" -> data
  ]
]


LesHouchesDataObject[
  "XSection", 
  assoc_? xsectionDataQ, 
  opts : OptionsPattern[]
][args___] := Failure[
  "UnknownArgument",
  Association[
    "Message" -> "Unrecognized argument sequence for cross section table LesHouchesDataObject.",
    "Input" -> {args},
    "Target" -> LesHouchesDataObject["Block", assoc, opts]
  ]
]


(* ::Subsubsubsection::Closed:: *)
(*boxes*)


LesHouchesDataObject /: MakeBoxes[
  obj : LesHouchesDataObject["XSection", assoc_? xsectionDataQ, opts : OptionsPattern[]], 
  form : (StandardForm | TraditionalForm)
] := With[
  {
   above = {
     {
      BoxForm`SummaryItem @ {"XSection ", assoc @ "ProcessRule"}, 
      BoxForm`SummaryItem @ {"\!\(\*SqrtBox[\(s\)]\) = ", assoc @ "CenterOfMassEnergy"}
     },
     {
      BoxForm`SummaryItem @ {"Description: ", Replace[assoc @ "Description", _Missing -> None]}, 
      SpanFromLeft
     }
   },
    
   below = If[
     MissingQ[obj @ "Dataset"],
     {BoxForm`SummaryItem @ {"Data: ", None}},
     {BoxForm`SummaryItem @ {obj @ "Dataset"}}
   ]
  },
  
  BoxForm`ArrangeSummaryBox[
    LesHouchesDataObject,
    obj,
    None,
    above,
    below,
    form,
    "Interpretable" -> Automatic
  ]
  
]


(* ::Chapter:: *)
(*end package*)


(* ::Subsubsection::GrayLevel[0]::Closed:: *)
(*end private context*)


End[];


(* ::Subsubsection::GrayLevel[0]::Closed:: *)
(*end package context*)


EndPackage[];
