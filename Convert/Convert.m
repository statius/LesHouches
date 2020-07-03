(* ::Package:: *)

(* ::Title:: *)
(*LesHouches`Convert`*)


(* ::Subsubsection:: *)
(*metadata*)


(* :Title: LesHouches *)
(* :Author: Andrew Miller <amiller@physics.umn.edu> *)
(* :Context: LesHouches`Convert` *)
(* :Version: 1.0.0 *)
(* :Date: 2016-11-01 *)
(* :Mathematica Version: 10.2 *)
(* :Copyright: (c) 2016 Andrew Miller *)


(* ::Chapter::GrayLevel[0]:: *)
(*context LesHouches`Convert`*)


(* ::Subsubsection::GrayLevel[0]::Closed:: *)
(*begin context*)


Begin @ "LesHouches`Convert`";


(* ::Section:: *)
(*utilities*)


(* ::Subsection:: *)
(*string parsers*)


(* ::Subsubsection::Closed:: *)
(*stringLines*)


stringLines[s_String] := Module[
  {stream, lines},
  
  stream = StringToStream @ s;
  lines = ReadList[stream, Record];
  Close @ stream;
  lines
  
]


(* ::Subsubsection::Closed:: *)
(*interpretStringReal*)


(* see this Mathematica Stack Exchange post for a discussion of string to real number interpretation: http://mathematica.stackexchange.com/questions/1737 *)


interpretStringReal[] := Missing @ "NoInput"


interpretStringReal[""] := Missing @ "NoInput"


interpretStringReal[s_String] := System`Convert`TableDump`ParseTable[
  {{s}},
  {{{}, {}}, {"-", "+"}, "."},
  False
][[1, 1]]


(* ::Section:: *)
(*import processors*)


(* ::Subsection:: *)
(*iLesHouchesImportProcessor*)


(* ::Subsubsection:: *)
(*block*)


(* ::Subsubsubsection:: *)
(*"Header"*)


iLesHouchesImportProcessor["Block", "Header"][lines : {__String}] := With[
  {
   header = Select[lines, StringStartsQ["block", IgnoreCase -> True]]
  },
  
  If[
    Length @ header > 0, 
    
    DeleteCases[
      StringCases[
        First @ header,
        
        RegularExpression["^(?i)(block) +([^ #]+)( +q *= *([^ #]+))* *(#(.*))?$"] :> Sequence[
          "Name" -> "$2",
          "RenormalizationScale" -> interpretStringReal @ "$4",
          "Description" -> StringTrim @ "$6"
        ]
      ] // Association,
      
      (_Missing | "")
    ],
    
    (*
    DeleteCases[
      Association[
        {
         "Name" \[Rule] StringSplit[First @ header]\[LeftDoubleBracket]2\[RightDoubleBracket],
         StringCases[First @ header, RegularExpression["[Qq] *= *(\\S+)"] \[RuleDelayed] ("RenormalizationScale" \[Rule] interpretStringReal @ "$1")],
         StringCases[First @ header, RegularExpression["#(.+)$"] \[RuleDelayed] ("Description" \[Rule] StringTrim @ "$1")]
        } // Flatten
      ],
      
      (_Missing | "")
    ],
    *)
    
    Association @ header
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"RawData"*)


iLesHouchesImportProcessor["Block", "RawData"][lines : {__String}] := With[
  {
   data = Select[lines, StringStartsQ[Whitespace, IgnoreCase -> True]]
  },

  (* split integers off of beginning of lines 
     split descriptions out of data by first "#" character 
     interpret key as integers necessary to leave at least one integer, number, or string for the value *)
 
  FixedPoint[
    Replace[
      #,
      {
       {i : {__Integer}, _Missing, d_} :> {Most @ i, Last @ i, d}, 
       ("" | {}) -> Missing @ "NotAvailable", 
       {x_} :> x
      },
      {1, \[Infinity]}
    ] &,
    
    StringCases[
      data,
      RegularExpression["^ +((\\d+ +)*)([^#]*)(#(.*))?$"] :> Sequence[
        ToExpression[StringSplit @ "$1"], 
        interpretStringReal[StringTrim @ "$3"], 
        StringTrim @ "$5"
      ]
    ]  
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"RawData", "ColumnNames"*)


iLesHouchesImportProcessor["Block", "RawData", "ColumnNames"][lines : {__String}] := With[
  {
   data = iLesHouchesImportProcessor["Block", "RawData"][lines]
  },
  
  If[
    Length @ data > 0, 
    {"Key", "Value", "Description"},
    data
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"Entries"*)


(* ::Input:: *)
(*iLesHouchesImportProcessor["Block", "Entries"][lines : {__String}] := With[*)
(*  {*)
(*   data = iLesHouchesImportProcessor["Block", "RawData"][lines]*)
(*  },*)
(*  *)
(*  Replace[*)
(*    data,*)
(*    {*)
(*     {Null, y_, Null} :> {"Value" -> y},*)
(*     {Null, y_, z_} :> {"Value" -> y, "Description" -> z},*)
(*     {x_, y_, Null} :> (x -> {"Value" -> y}),*)
(*     {x_, y_, z_String} :> (x -> {"Value" -> y, "Description" -> z})*)
(*    },*)
(*    1*)
(*  ]  *)
(*  *)
(*]*)


(* ::Subsubsection:: *)
(*decay*)


(* ::Subsubsubsection::Closed:: *)
(*"Header"*)


iLesHouchesImportProcessor["Decay", "Header"][lines : {__String}] := With[
  {
   header = Select[lines, StringStartsQ["decay", IgnoreCase -> True]]
  },
  
  If[
    Length @ header > 0, 
    
    DeleteCases[
      StringCases[
        First @ header,
        
        RegularExpression["^(?i)(decay) +(-?\\d+) +([^ #]+) *(#(.*))?$"] :> Sequence[
          "InitialState" -> ToExpression @ "$2",
          "TotalWidth" -> interpretStringReal @ "$3",
          "Description" -> StringTrim @ "$5"
        ]
      ] // Association,
      
      (_Missing | "")
    ],
    
    (*
    DeleteCases[
      Association[
        {
         "InitialState" \[Rule] StringSplit[First @ header]\[LeftDoubleBracket]2\[RightDoubleBracket],
         "TotalWidth" \[Rule] interpretStringReal[StringSplit[First @ header]\[LeftDoubleBracket]3\[RightDoubleBracket]],
         StringCases[First @ header, RegularExpression["#(.+)$"] \[RuleDelayed] ("Description" \[Rule] StringTrim @ "$1")]
        } // Flatten
      ],
      
      (_Missing | "")
    ],
    *)
    
    Association @ header
  ]
  
]


(* ::Subsubsubsection::Closed:: *)
(*"RawData"*)


iLesHouchesImportProcessor["Decay", "RawData"][lines : {__String}] := With[
  {
   data = Select[lines, StringStartsQ[Whitespace, IgnoreCase -> True]]
  },
  
  StringCases[
    data,
    RegularExpression["^ +([^ ]+) +\\d+ +((-?\\d+ +)*)(-?\\d+) *(#(.*))?$"] :> Sequence[
      interpretStringReal[StringTrim @ "$1"], 
      ToExpression[Join[StringSplit @ "$2", {"$4"}]],
      StringTrim @ "$6"
    ]
  ] /. "" -> Missing @ "NotAvailable"
  
]


(* ::Subsubsubsection::Closed:: *)
(*"RawData", "ColumnNames"*)


iLesHouchesImportProcessor["Decay", "RawData", "ColumnNames"][lines : {__String}] := With[
  {
   data = iLesHouchesImportProcessor["Decay", "RawData"][lines]
  },
  
  If[
    Length @ data > 0, 
    {"BranchingRatio", "FinalStates", "Description"},
    data
  ]
  
]


(* ::Subsubsection:: *)
(*xsection*)


(* ::Subsubsubsection::Closed:: *)
(*"Header"*)


iLesHouchesImportProcessor["XSection", "Header"][lines_] := With[
  {
   header = Select[lines, StringStartsQ["xsection", IgnoreCase -> True]]
  },
  
  If[
    Length @ header > 0, 
    
    DeleteCases[
      StringCases[
        First @ header,
        
        RegularExpression["^(?i)(xsection) +([^ ]+) +(-?\\d+) +(-?\\d+) +((-?\\d+) +)*(-?\\d+) *(#(.*))?$"] :> Sequence[
          "CenterOfMassEnergy" -> interpretStringReal @ "$2",
          "InitialStates" -> Sort[ToExpression @ {"$3", "$4"}],
          "FinalStates" -> Sort @ ToExpression[Join[StringSplit @ "$5", {"$7"}]],
          "ProcessRule" -> (Sort[ToExpression @ {"$3", "$4"}] -> Sort @ ToExpression[Join[StringSplit @ "$5", {"$7"}]]),
          "Description" -> StringTrim @ "$9"
        ]
      ] // Association,
      
      (_Missing | "")
    ],
    
    Association @ header
  ]
   
]


(* ::Subsubsubsection::Closed:: *)
(*"RawData"*)


iLesHouchesImportProcessor["XSection", "RawData"][lines : {__String}] := With[
  {
   data = Select[lines, StringStartsQ[Whitespace, IgnoreCase -> True]]
  },
  
  StringCases[
    data,
    RuleDelayed[
      RegularExpression @ "^ +(\\d+) +(\\d+) +(\\d+) +([^ ]+) +([^ ]+) +(\\d+) +([^ ]+) +([^ ]+) +([^ #]+) *(#(.*))?$",
      (*
      StringExpression[
        StartOfString,
        Whitespace,
        scale : integerString,
        Whitespace,
        qcd : integerString,
        Whitespace,
        ew : integerString,
        Whitespace,
        kappar: (Except[" "] .. ),
        Whitespace,
        kappaf: (Except[" "] .. ),
        Whitespace,
        pdf: integerString,
        Whitespace,
        v : (Except[" "] .. ),
        Whitespace,
        code : (Except[" "] .. ),
        Whitespace,
        ver : (Except[" "] .. ),
        Whitespace | "",
        ("#" ~~ Longest[d__])| "",
        EndOfString
      ],
      *)
      Sequence[
        ToExpression @ "$1", 
        ToExpression @ "$2", 
        ToExpression @ "$3", 
        interpretStringReal @ "$4", 
        interpretStringReal @ "$5",
        ToExpression @ "$6",
        interpretStringReal @ "$7",
        "$8",
        "$9",
        StringTrim @ "$11"
      ]
    ]
  ] /. "" -> Missing @ "NotAvailable"
   
]


(* ::Subsubsubsection::Closed:: *)
(*"RawData", "ColumnNames"*)


iLesHouchesImportProcessor["XSection", "RawData", "ColumnNames"][lines : {__String}] := With[
  {
   data = Select[lines, StringStartsQ[Whitespace, IgnoreCase -> True]]
  },
  
  If[
    Length @ data > 0, 
    
    {
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
    },
    
    data
  ]
  
]


(* ::Subsubsection:: *)
(*all*)


(* ::Subsubsubsection::Closed:: *)
(*data*)


iLesHouchesImportProcessor[
  structure : ("Block" | "Decay" | "XSection"), 
  "Data"
][lines : {__String}] := DeleteMissing[
  AssociationThread[
    iLesHouchesImportProcessor[structure, "RawData", "ColumnNames"][lines],
    #
  ] & /@ iLesHouchesImportProcessor[structure, "RawData"][lines], 
  2
]


(* ::Subsubsubsection::Closed:: *)
(*whole structure*)


iLesHouchesImportProcessor[structure : ("Block" | "Decay" | "XSection")][lines : {__String}] := LesHouchesDataObject[
  structure,
  DeleteCases[
    Association[
      iLesHouchesImportProcessor[structure, "Header"][lines],
      "Data" -> iLesHouchesImportProcessor[structure, "Data"][lines]
    ],
    {}
  ]
]


(* ::Subsection:: *)
(*LesHouchesImportProcessor*)


(* ::Subsubsection::Closed:: *)
(*all*)


LesHouchesImportProcessor[
  structure : ("Block" | "Decay" | "XSection")
][lines : {__String}] := iLesHouchesImportProcessor[structure][lines]


LesHouchesImportProcessor[
  structure : ("Block" | "Decay" | "XSection")
][data_String] := iLesHouchesImportProcessor[structure][stringLines @ data]


LesHouchesImportProcessor[
  structure : ("Block" | "Decay" | "XSection")
][data : {{__String} .. }] := Thread[Unevaluated[LesHouchesImportProcessor[structure][data]]]


LesHouchesImportProcessor[
  structure : ("Block" | "Decay" | "XSection"), 
  args__
][lines : {__String}] := With[
  {
   object = iLesHouchesImportProcessor[structure][lines]
  },
  
  If[
    ! FailureQ @ object,
    object @ args,
    object
  ]
  
]


LesHouchesImportProcessor[
  structure : ("Block" | "Decay" | "XSection"), 
  args__
][data_String] := iLesHouchesImportProcessor[structure][stringLines @ data][args]


LesHouchesImportProcessor[
  structure : ("Block" | "Decay" | "XSection"), 
  args__
][data : {{__String} .. }] := Thread[Unevaluated[LesHouchesImportProcessor[structure, args][data]]]


LesHouchesImportProcessor[("Block" | "Decay" | "XSection"), args___][{}] := {}


LesHouchesImportProcessor[("Block" | "Decay" | "XSection"), args___][data_? FailureQ] := data


(* ::Section:: *)
(*import elements*)


(* ::Subsection:: *)
(*iLesHouchesImport*)


(* ::Subsubsection:: *)
(*file*)


(* ::Subsubsubsection::Closed:: *)
(*"Summary"*)


iLesHouchesImport["Summary"][filename_String, opts___] := With[
  {
   blocks = iLesHouchesImport["Block", "Names"][filename, opts], 
   decays = iLesHouchesImport["Decay", "InitialStates"][filename, opts], 
   xsecs =iLesHouchesImport["XSection", "ProcessRules"][filename, opts]
  },

  Association[
    "BlockCount" -> Length @ blocks,
    "BlockNames" -> blocks, 
    "DecayCount" -> Length @ decays,
    "DecayStates" -> decays,
    "XSectionCount" -> Length @ xsecs,
    "XSectionProcesses" -> xsecs
  ] // Dataset
]


(* ::Subsubsubsection::Closed:: *)
(*"Plaintext"*)


iLesHouchesImport["Plaintext"][filename_String, opts___] := With[
  {
   text = StringCases[
   
     (* delete comment lines *)
     StringDelete[
       Import[filename, "Text"], 
       RegularExpression@"(?m)^#[^\\n]*\\n"
       (*(StartOfString | "\n") ~~ "#" ~~ Longest[Except["\n"] ...]*)
     ],
     
     RegularExpression["(?i)(?m)(?s)^(((block)|(decay)|(xsection)).+?)((\\nblock)|(\\ndecay)|(\\nxsection))"] :> "$1",
     (*s : Shortest[("block" | "decay" | "xsection") ~~ __] ~~ ("\nblock" | "\ndecay" | "\nxsection" | EndOfString) \[RuleDelayed] s,*)
     
     IgnoreCase -> True,
     Overlaps -> True
   ] // StringTrim
  },

  With[
    {
     blocks = Select[text, StringStartsQ["block", IgnoreCase -> True]],
     decays = Select[text, StringStartsQ["decay", IgnoreCase -> True]],
     xsects = Select[text, StringStartsQ["xsection", IgnoreCase -> True]]
    },

    {"Blocks" -> blocks, "Decays" -> decays, "XSections" -> xsects}
  
  ]
]


(* ::Subsubsubsection::Closed:: *)
(*all (default import function)*)


iLesHouchesImport[All][filename_String, opts___] := Module[
  {lines, blocks, decays, xsects},

  lines = ReadList[filename, Record];

  lines = Select[lines, StringStartsQ["block" | "decay" | "xsection" | WhitespaceCharacter, IgnoreCase -> True]];

  lines = Module[{i = 0}, GatherBy[lines, (If[StringStartsQ[#, LetterCharacter], ++i, i]) &]];

  blocks = LesHouchesImportProcessor["Block"][Select[lines, StringStartsQ[First @ #, "block", IgnoreCase -> True] &]];
  decays = LesHouchesImportProcessor["Decay"][Select[lines, StringStartsQ[First @ #, "decay", IgnoreCase -> True] &]];
  xsects = LesHouchesImportProcessor["XSection"][Select[lines, StringStartsQ[First @ #, "xsection", IgnoreCase -> True] &]];

  If[
    MatchQ[{blocks, decays, xsects}, {{}, {}, {}}],
    
    Message[Import::fmterr, "Les Houches Accord"];
    $Failed,
    
    {"Blocks" -> blocks, "Decays" -> decays, "XSections" -> xsects}
  ]
]


(* ::Subsubsection:: *)
(*block*)


(* ::Subsubsubsection::Closed:: *)
(*"Names"*)


iLesHouchesImport[
  "Block", 
  "Names"
][filename_, opts___] := (StringSplit @ FindList[filename, "bloc", AnchoredSearch -> True, IgnoreCase -> True])[[All, 2]]


(* ::Subsubsubsection::Closed:: *)
(*"Elements"*)


iLesHouchesImport[
  "Block", 
  "Elements"
][filename_, opts___] := Prepend[
  iLesHouchesImport["Block", "Names"][filename, opts] // Sort, 
  "Names"
]


(* ::Subsubsubsection::Closed:: *)
(*"Lines"*)


iLesHouchesImport[
  "Block", 
  All, 
  "Lines"
][filename_, opts___] := iLesHouchesImport["Block", #, "Lines"][filename, opts] & /@ iLesHouchesImport["Block", "Names"][filename, opts]


iLesHouchesImport[
  "Block", 
  blockname_, 
  "Lines"
][filename_, opts___] := Module[
  {stream, titles, blocks, title, data},

  (* find matching block(s) *)
  titles = FindList[filename, "bloc", AnchoredSearch -> True, IgnoreCase -> True];
  
  If[
    Length @ titles < 1,
    Message[Import::"noelem", "Blocks", "LesHouches"];
    $Failed // Return
  ];
  
  titles = Select[
    titles, 
    
    StringMatchQ[
      #, 
      (*"block" ~~ Whitespace ~~ ToUpperCase @ blockname ~~ ("" | (Whitespace ~~ __)),*)
      RegularExpression["(?i)^(block) +(" <> ToLowerCase @ blockname <> ").*$"], 
      IgnoreCase -> True
    ] &
  ];
  
  (* read data *)
  If[
    Length @ titles > 0,
    
    blocks = Function[
      stream = OpenRead @ filename;
      
      title = Find[stream, #];
      
      data = NestWhileList[
        Read[stream, Record] &, 
        Read[stream, Record], 
        And[UnsameQ[#, EndOfFile], StringStartsQ[#, (" " | "#")]] &
      ];
      
      data = Select[Most @ data, StringStartsQ[" "]];
      
      Join[{title}, data]
    ] /@ titles;

    Close @ stream;

    blocks,
    
    Message[Import::noelem, {"Blocks", blockname}, "LesHouches"];
    $Failed
  ]

]


(* ::Subsubsection:: *)
(*decay*)


(* ::Subsubsubsection::Closed:: *)
(*"InitialStates"*)


iLesHouchesImport[
  "Decay", 
  "InitialStates"
][filename_, opts___] := ToExpression[(StringSplit @ FindList[filename, "deca", AnchoredSearch -> True, IgnoreCase -> True])[[All, 2]]]


(* ::Subsubsubsection::Closed:: *)
(*"Elements"*)


iLesHouchesImport[
  "Decay", 
  "Elements"
][filename_, opts___] := Prepend[
  iLesHouchesImport["Decay", "InitialStates"][filename, opts] // Sort, 
  "InitialStates"
]


(* ::Subsubsubsection::Closed:: *)
(*"Lines"*)


iLesHouchesImport[
  "Decay", 
  All, 
  "Lines"
][filename_, opts___] := iLesHouchesImport["Decay", #, "Lines"][filename, opts] & /@ iLesHouchesImport["Decay", "InitialStates"][filename, opts]


iLesHouchesImport[
  "Decay", 
  pdgnumber_, 
  "Lines"
][filename_, opts___] := Module[
  {stream, titles, decays, title, data},

  (* find matching decay table(s) and set stream position accordingly *)
  titles = FindList[filename, "deca", AnchoredSearch -> True, IgnoreCase -> True];
  
  If[
    Length @ titles < 1,
    Message[Import::"noelem", "Decays", "LesHouches"];
    $Failed // Return
  ];
   
  titles = Select[
    titles, 
    
    StringMatchQ[
      #, 
      (*"decay" ~~ Whitespace ~~ ToString@pdgnumber ~~ Whitespace ~~ ___,*)
      RegularExpression["(?i)^(decay) +(" <> IntegerString @ pdgnumber <> ") +[^ #]+.*"], 
      IgnoreCase -> True
    ] &
  ];
  
  (* read data *)
  If[
    Length @ titles > 0,
    
    decays = Function[
      stream = OpenRead @ filename;
      
      title = Find[stream, #];
      
      data = NestWhileList[
        Read[stream, Record] &, 
        Read[stream, Record], 
        And[UnsameQ[#, EndOfFile], StringStartsQ[#, (" " | "#")]] &
      ];
      
      data = Select[Most @ data, StringStartsQ[" "]];
      
      Join[{title}, data]
    ] /@ titles;

    Close @ stream;

    decays,
    
    Message[Import::noelem, {"Decays", pdgnumber}, "LesHouches"];
    $Failed
  ]

]


(* ::Subsubsection:: *)
(*xsection*)


(* ::Subsubsubsection::Closed:: *)
(*"ProcessRules"*)


iLesHouchesImport[
  "XSection", 
  "ProcessRules"
][filename_, opts___] := With[
  {
   lines = StringSplit @ StringSplit[FindList[filename, "xsectio", AnchoredSearch -> True, IgnoreCase -> True], "#"][[All, 1]]
  },
  
  Map[Sort, Thread[ToExpression @ lines[[All, {3, 4}]] -> ToExpression @ lines[[All, 6 ;; ]]], {2}] // Union
  
]


(* ::Subsubsubsection::Closed:: *)
(*"Elements"*)


iLesHouchesImport[
  "XSection", 
  "Elements"
][filename_, opts___] := Prepend[
  iLesHouchesImport["XSection", "ProcessRules"][filename, opts],
  "ProcessRules"
]


(* ::Subsubsubsection::Closed:: *)
(*"Lines"*)


iLesHouchesImport[
  "XSection", 
  All, 
  "Lines"
][filename_, opts___] := Map[
  iLesHouchesImport["XSection", #, "Lines"][filename, opts] &,
  Union[Sort /@ iLesHouchesImport["XSection", "ProcessRules"][filename, opts][[All, 1]]]
]


iLesHouchesImport[
  "XSection", 
  states_, 
  "Lines"
][filename_String, opts___] := Module[
  {stream, titles, xsecs, title, data},

  (* find matching xsection table(s) *)
  titles = FindList[filename, "xsectio", AnchoredSearch -> True, IgnoreCase -> True];
  
  If[
    Length @ titles < 1,
    Message[Import::noelem, "XSections", "LesHouches"];
    $Failed // Return
  ];
   
  title = Take[StringSplit @ StringSplit[titles, "#"][[All, 1]], {3, 4}];
  
  titles = Select[titles, ContainsAll[ToExpression[#], states] &];
  
  (* read data *)
  If[
    Length @ titles > 0,
    
    xsecs = Function[
      stream = OpenRead @ filename;
    
      title = Find[stream, #];
      
      data = NestWhileList[
        Read[stream, Record] &, 
        Read[stream, Record], 
        And[UnsameQ[#, EndOfFile], StringStartsQ[#, (" " | "#")] &]
      ];
      
      data = Select[Most @ data, StringStartsQ[" "]];
      
      Join[{title}, data]
    ] /@ titles;

    Close @ stream;

    xsecs,
    
    Message[Import::noelem, {"XSections", states}, "LesHouches"];
    $Failed
  ]  
  
]


(* ::Subsubsection:: *)
(*general elements*)


(* ::Subsubsubsection::Closed:: *)
(*"Rules"*)


iLesHouchesImport[
  structure : ("Block" | "Decay" | "XSection"), 
  "Rules"
][filename_, opts___] := AssociationMap[
  Replace[iLesHouchesImport[structure, #][filename, opts], {x_} :> x] &,
  iLesHouchesImport[structure, "Elements"][filename, opts]
] // Normal


(* ::Subsubsubsection::Closed:: *)
(*all*)


iLesHouchesImport[
  structure : ("Block" | "Decay" | "XSection"), 
  All
][filename_, opts___] := iLesHouchesImport[structure, #][filename, opts] & /@ iLesHouchesImport[structure, "Elements"][filename, opts]


(* ::Subsubsubsection::Closed:: *)
(*structure by specification*)


iLesHouchesImport[
  structure : ("Block" | "Decay" | "XSection"), 
  spec_
][filename_, opts___] := With[
  {
   lines = iLesHouchesImport[structure, spec, "Lines"][filename, opts]
  },
  
  If[
    ! FailureQ @ lines,
  
    With[
      {
       objs = LesHouchesImportProcessor[structure][lines]
      },
      
      If[
        ! FailureQ @ #,
        #,
        Message[Import::elemerr, {structure <> "s", spec}];
        $Failed
      ] & /@ objs
    
    ],
    
    lines
  ]
  
]


iLesHouchesImport[
  structure : ("Block" | "Decay" | "XSection"), 
  spec_, 
  args__
][filename_, opts___] := With[
  {
   objs = iLesHouchesImport[structure, spec][filename, opts]
  },
  
  If[
    ! FailureQ @ #,
  
    With[
      {
       target = # @ args
      },
      
      If[
        ! FailureQ @ target,
        target,
        Message[Import::elemerr, {structure <> "s", spec, args}];
        $Failed
      ]
    
    ],
    
    #
  ] & /@ objs
  
]


(* ::Subsection:: *)
(*LesHouchesImport*)


(* ::Subsubsection:: *)
(*file*)


(* ::Subsubsubsection::Closed:: *)
(*"Summary"*)


LesHouchesImport["Summary"][filename_String, opts___] := "Summary" -> iLesHouchesImport["Summary"][filename, opts]


(* ::Subsubsubsection::Closed:: *)
(*"Plaintext"*)


LesHouchesImport["Plaintext"][filename_String, opts___] := "Plaintext" -> iLesHouchesImport["Plaintext"][filename, opts]


(* ::Subsubsubsection::Closed:: *)
(*"DecayStates"*)


LesHouchesImport["DecayStates"][filename_, opts___] := "DecayStates" -> iLesHouchesImport["Decay", "InitialStates"][filename, opts]


(* ::Subsubsubsection::Closed:: *)
(*"BlockNames"*)


LesHouchesImport["BlockNames"][filename_, opts___] := "BlockNames" -> iLesHouchesImport["Block", "Names"][filename, opts]


(* ::Subsubsubsection::Closed:: *)
(*"XSectionProcesses"*)


LesHouchesImport["XSectionProcesses"][filename_, opts___] := "XSectionProcesses" -> iLesHouchesImport["XSection", "ProcessRules"][filename, opts]


(* ::Subsubsubsection::Closed:: *)
(*all (default import function)*)


LesHouchesImport[All][filename_String, opts___] := iLesHouchesImport[All][filename, opts]


(* ::Subsubsection:: *)
(*general elements*)


(* ::Subsubsubsection::Closed:: *)
(*rule folder*)


LesHouchesImport[
  structure : ("Block" | "Decay" | "XSection"), 
  args__
][filename_String, opts___] := Fold[
  {Rule[#2, #1]} &, 
  {
   structure <> "s", 
   args, 
   Replace[iLesHouchesImport[structure, args][filename, opts], {x_} :> x]
  } // Reverse
]


(* ::Chapter:: *)
(*end LesHouches`Convert` context*)


(* ::Subsubsection::GrayLevel[0]::Closed:: *)
(*end context*)


End[];
