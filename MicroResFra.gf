resource MicroResFra = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Gender = Fem | Masc ;


  Agreement = Agr Number ; ---s Person to be added

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
    x + "e"			=> mkAdj sg (x + "") (x + "s") (x + "s") ;
    x + "ieux"			=> mkAdj sg (x + "ielle") (x + "eux") (x + "ieilles") ;
    x + "eux"			=> mkAdj sg (x + "euse") (x + "eux") (x + "euses") ;
    x + "eau"			=> mkAdj sg (x + "elle") (x + "aux") (x + "elles") ;
    _ 				=> regAdj sg
    } ;

  Verb : Type = {s : Person => Number => Str} ;

  mkVerb : (Inf,p1sg,p1pl,p2sg,p2pl,p3sg,p3pl : Str) -> Verb
    = \Inf,p1sg,p1pl,p2sg,p2pl,p3sg,p3pl -> {
    s = table {
      P1 => table { Sg => p1sg ; Pl => p1pl } ;
      P2 => table { Sg => p2sg ; Pl => p2pl } ;
      P3 => table { Sg => p3sg ; Pl => p3pl } 
      }
    } ;

  --regVerb : (inf : Str) -> Verb = \inf ->
    --mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \stem -> case stem of {
     stem + "eter" 		=> verb4eter stem ;
     stem + "er" 			=> verb1er stem ;
     stem + "ir"			=> verb2ir stem ; 
     stem + "re"			=> verb3re stem 
--     _ => regVerb inf
     } ;

  verb1er: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "e" ; Pl => stem + "ons" } ;
      P2 => table { Sg => stem + "es" ; Pl => stem + "ez" } ;
      P3 => table { Sg => stem + "e" ; Pl => stem + "ent" } 
      }
    } ;

  verb2ir: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "s" ; Pl => stem + "ons" } ;
      P2 => table { Sg => stem + "s" ; Pl => stem + "ez" } ;
      P3 => table { Sg => stem + "t" ; Pl => stem + "ent" } 
      }
    } ;

  verb3re: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "s" ; Pl => stem + "ons" } ;
      P2 => table { Sg => stem + "s" ; Pl => stem + "ez" } ;
      P3 => table { Sg => stem + "" ; Pl => stem + "ent" } 
      }
    } ;

  verb4eter: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "ète" ; Pl => stem + "ètons" } ;
      P2 => table { Sg => stem + "ètes" ; Pl => stem + "ètez" } ;
      P3 => table { Sg => stem + "ète" ; Pl => stem + "ètent" } 
      }
    } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  -- irregVerb : (inf,past,pastpart : Str) -> Verb =
  --  \inf,past,pastpart ->
  --    let verb = smartVerb inf
  --    in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "être" "suis" "sommes" "es" "êtes" "est" "sont" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  --agr2vform : Agreement -> VForm = \a -> case a of {
    --Agr Sg => PresSg3 ;
    --Agr Pl => Inf
    --} ;

}