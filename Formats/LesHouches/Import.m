(* ::Package:: *)

ImportExport`RegisterImport[
  "LesHouches",

  (* importer elements *)
  {
   "Summary" :> LesHouches`Convert`LesHouchesImport["Summary"],
   "Plaintext" :> LesHouches`Convert`LesHouchesImport["Plaintext"],
   
   "BlockNames" :> LesHouches`Convert`LesHouchesImport["BlockNames"],
   
   {"Blocks", elem_String} :> LesHouches`Convert`LesHouchesImport["Block", elem],
   {"Blocks", elem_String, args__} :> LesHouches`Convert`LesHouchesImport["Block", elem, args],
   
   "DecayStates" :> LesHouches`Convert`LesHouchesImport["DecayStates"],
   
   {"Decays", elem_String} :> LesHouches`Convert`LesHouchesImport["Decay", elem],
   {"Decays", pdgnumber_Integer} :> LesHouches`Convert`LesHouchesImport["Decay", pdgnumber],
   {"Decays", pdgnumber_Integer, args__} :> LesHouches`Convert`LesHouchesImport["Decay", pdgnumber, args],
   
   "XSectionProcesses" :> LesHouches`Convert`LesHouchesImport["XSectionProcesses"],
   
   {"XSections", elem_String} :> LesHouches`Convert`LesHouchesImport["XSection", elem],
   {"XSections", states : Repeated[_Integer, {1, 2}]} :> LesHouches`Convert`LesHouchesImport["XSection", {states}],
   {"XSections", states : Repeated[_Integer, {1, 2}], args__} :> LesHouches`Convert`LesHouchesImport["XSection", {states}, args],
   
   (* default import function *)
   LesHouches`Convert`LesHouchesImport[All]
  },
]
