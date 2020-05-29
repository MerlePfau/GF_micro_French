--# -path=.:../abstract
concrete MicroLangFra of MicroLang = open MicroResFra, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Str} ; ---s special case of Mini
    Comp = Adjective ;
    AP = Adjective ;
    NP = {s : Str ; n : Number; p : Person};
    Pron = {s : Case => Str ; n : Number; p : Person} ;
    Det = {s : Str ; n : Number} ;
    Prep = {s : Str} ;
    V = {s : Person => Number => Str} ;
    V2 = Verb2 ;
    A = Adjective ;
    N, CN = Noun ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s} ;

    PredVPS np vp = {
      s = np.s ++ vp.verb.s ! np.p ! np.n ++ vp.compl
      } ;
      
    UseV v = {
      verb = v ;
      compl = [] ;
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = v2.c ++ np.s  -- NP object in the accusative, preposition first
      } ;
      
    --UseComp comp = {
    --  verb = be_Verb ;     -- the verb is the copula "be"
    --  compl = \\g => comp.s ! n ! g ;
    --  } ;
      
    CompAP ap = ap ;
      
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
      
    DetCN det cn = {
      s = det.s ++ cn.s ! det.n ;
      g = cn.g ;
      n = det.n ;
      p = P3
      } ;
      
    -- UsePron p = p.s ;
            
    a_Det = {s = "une" ; n = Sg} ; 
    aPl_Det = {s = "des" ; n = Pl} ;
    the_Det = {s = pre {"a"|"e"|"i"|"o"|"h" => "l'" ; _ => "la"} ; n = Sg} ;
    thePl_Det = {s = "elles" ; n = Pl} ;
    
    UseN n = n ;    

    AdjCN ap cn = {
      s = \\n => ap.s ! cn.g ! n ++ cn.s ! n ;
      g = cn.g ;
      } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s } ;

    in_Prep = {s = "dans"} ;
    on_Prep = {s = "sur"} ;
    with_Prep = {s = "avec"} ;

    he_Pron = {
      s = table {Nom => "il" ; Acc => "le"} ;
      n = Sg ;
      p = P3 ;
      } ;
    she_Pron = {
      s = table {Nom => "elle" ; Acc => "la"} ;
      n = Sg ;
      p = P3 ;
      } ;
    they_Pron = {
      s = table {Nom => "elles" ; Acc => "les"} ;
      n = Pl ;
      p = P3 ;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "déjà" ;
lin animal_N = mkN "animal" ;  --m
lin apple_N = mkN "pomme" ;  --m
lin baby_N = mkN "bébé" ; --m
lin bad_A = mkA "mal" ;
lin beer_N = mkN "bière" ;  --f
lin big_A = mkA "grand" ;
lin bike_N = mkN "vélo" ;  --m
lin bird_N = mkN "oiseau" ;  --m
lin black_A = mkA "noir" ;
lin blood_N = mkN "sang" ;  --m
lin blue_A = mkA "bleu" ;
lin boat_N = mkN "bateau" ;  --m
lin book_N = mkN "livre" ;  --m
lin boy_N = mkN "garçon" ;  --m
lin bread_N = mkN "pain" ;  --m
lin break_V2 = mkV2 (mkV "casser") ;
lin buy_V2 = mkV2 (mkV "acheter") ;
lin car_N = mkN "voiture" ;  --f
lin cat_N = mkN "chat" ;  --m
lin child_N = mkN "enfant" ;  --m
lin city_N = mkN "ville" ;  --l
lin clean_A = mkA "propre" ;
lin clever_A = mkA "malin" ;
lin cloud_N = mkN "nuage" ;  --m
lin cold_A = mkA "froid" ;
lin come_V = mkV "venir" "viens" "viens" "vient" "venons" "venez" "viennent" ;
lin computer_N = mkN "ordinateur" ;  --m
lin cow_N = mkN "vache" ;  --f
lin dirty_A = mkA "sale" ;
lin dog_N = mkN "chien" ;  --m
lin drink_V2 = mkV2 (mkV "boire") ;
lin eat_V2 = mkV2 (mkV "manger") ;
lin find_V2 = mkV2 (mkV "trouver") ;
lin fire_N = mkN "feu" ;  --m
lin fish_N = mkN "poisson" ;  --m
lin flower_N = mkN "fleur" ;  --f
lin friend_N = mkN "amie" ;  --f
lin girl_N = mkN "fille" ;  --f
lin good_A = mkA "bon"  ;
lin go_V = mkV "aller" "vais" "vas" "va" "allons" "allez" "vont" ;
lin grammar_N = mkN "grammaire" ;  --f
lin green_A = mkA "vert" ;
lin heavy_A = mkA "lourd" ;
lin horse_N = mkN "cheval" ;  --m
lin hot_A = mkA "chaud" ;
lin house_N = mkN "maison" ;  --f
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "bondir" ;
lin kill_V2 = mkV2 "tuer" ;
-- lin know_VS = mkVS (mkV "know" "knew" "known") ;
lin language_N = mkN "langue" ;  --f
lin live_V = mkV "vivre" ;
lin love_V2 = mkV2 (mkV "aimer") ;
lin man_N = mkN "homme" ;  --m
lin milk_N = mkN "lait" ;  --m
lin music_N = mkN "musique" ;  --f
lin new_A = mkA "nouveau" ;
lin now_Adv = mkAdv "maintenant" ;
lin old_A = mkA "vieux" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "jouer" ;
lin read_V2 = mkV2 (mkV "lire") ;
lin ready_A = mkA "prêt" ;
lin red_A = mkA "rouge" ;
lin river_N = mkN "rivière" ;  --f
lin run_V = mkV "courir" ;
lin sea_N = mkN "océan" ;  --m
lin see_V2 = mkV2 (mkV "voir") ;
lin ship_N = mkN "navire" ;  --m
lin sleep_V = mkV "dormir" ;
lin small_A = mkA "petit" ;
lin star_N = mkN "étoile" ;  --f
lin swim_V = mkV "nager";
lin teach_V2 = mkV2 (mkV "apprendre") ;
lin train_N = mkN "train" ;  --m
lin travel_V = mkV "voyager" ;
lin tree_N = mkN "arbre" ;  --m
lin understand_V2 = mkV2 (mkV "entendre") ;
lin wait_V2 = mkV2 "attendre" ;
lin walk_V = mkV "marcher" ;
lin warm_A = mkA "chaud" ;
lin water_N = mkN "eau" ;  --f
lin white_A = mkA "blanc" ;
lin wine_N = mkN "vin" ;  --m
lin woman_N = mkN "femme" ;  --f
lin yellow_A = mkA "jaune" ;
lin young_A = mkA "jeune" ;


---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN : Str -> Noun   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
      = \n -> lin N (smartNoun n) ;
    mkN : Str -> Str -> Noun  -- irregular noun, e.g. man-men
      = \sg,pl -> lin N (mkNoun sg pl) ;
    } ;

  mkA : (ASgMasc : Str) -> A 
    = \ASgMasc -> lin A (smartAdj ASgMasc) ;

  mkV = overload {
    mkV : (stem: Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \stem -> lin V (smartVerb stem) 
   -- mkV : (inf,pres,part : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
   --   = \inf,pres,part -> lin V (irregVerb inf pres part) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}
