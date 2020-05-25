resource MicroResFra = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Gender = Fem | Masc ;


  Agreement = Agr Number ; ---s Person to be added

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  VForm = Inf | PresSg3 | Past | PastPart | PresPart ; 

  Person = P1 | P2 | P3 ;

oper
  Noun : Type = {s : Number => Str} ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl}
    } ;

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ;

  -- smart paradigm
  smartNoun : Str -> Noun = \sg -> case sg of {
    x + "au"                   => mkNoun sg (x + "aux") ;
    x + "al"                   => mkNoun sg (x + "aux") ;
    _                          => regNoun sg
    } ;

  Adjective : Type = {s : Gender => Number => Str} ;

  mkAdj : (ASgMasc,ASgFem,APlMasc,APlFem : Str) -> Adjective
    = \ASgMasc,ASgFem,APlMasc,APlFem -> {
    s = table {
      Masc => table { Sg => ASgMasc ; Pl => APlMasc } ;
      Fem  => table { Sg => ASgFem ; Pl => APlFem } 
      }
    } ;

  regAdj : (ASgMasc : Str) -> Adjective = \sg ->
    mkAdj sg (sg + "e") (sg + "s") (sg + "es") ;

  smartAdj : Str -> Adjective = \sg -> case sg of {
    x + "n"			=> mkAdj sg (x + "nne") (x + "ns") (x + "nnes") ;
    _ 				=> regAdj sg
    } ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pres,past,pastpart,prespart : Str) -> Verb
    = \inf,pres,past,pastpart,prespart -> {
    s = table {
      Inf => inf ;
      PresSg3 => pres ;
      Past => past ;
      PastPart => pastpart ;
      PresPart => prespart
      }
    } ;

  regVerb : (inf : Str) -> Verb = \inf ->
    mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
     pl  +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
     cr  +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") ;
     lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed") (lov + "ing") ;
     kis + ("s"|"sh"|"x"|"o") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed") (inf + "ing") ;
     _ => regVerb inf
     } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  irregVerb : (inf,past,pastpart : Str) -> Verb =
    \inf,past,pastpart ->
      let verb = smartVerb inf
      in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "are" "is" "was" "been" "being" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> VForm = \a -> case a of {
    Agr Sg => PresSg3 ;
    Agr Pl => Inf
    } ;

}