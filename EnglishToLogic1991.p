(* This is Pascale code of historic interest only. With the Macintosh,
the actual rules were stored in the resource fork of the application
file, hence, for example, kAdjectivesRSRCID = 14639; *)


unit EnglishToLogic;

{note, there is a kludge here. The resources for gRules is read too slowly and thus}
{makes the initialization slow.  So now it is read on demand April 30 91. Search for kludge}

{note binadj and passive verbs can be one or two words long}
(*restrict terms to length 1*)



interface
	uses
		DerImpNotes, 

{TCL, UStream, MFLispGlobals, LispUnit; }

		SysEqu, Traps, ULoMem, UMacAppUtilities, UPatch, UObject, UViewCoords, UFailure, UMemory, UMenuSetup, UList, PrintTraps, UAssociation, UMacApp, Errors, 

		UStream, {MFLispGlobals;}
		ULogicGlobals90, LispUnit, UFormulaIntf;

	const
		kAdjectivesRSRCID = 14639;
		kBinaryAdjsRSRCID = 12924;
		kIntransitive_VerbsRSRCID = 10193;
		kNamesRSRCID = 20647;
		kNounsRSRCID = 16040;
		kPassiveVerbsRSRCID = 24361;
		kPredSymbolizationRulesRSRCID = 6761;
		kPropsRSRCID = 24459;
		kPropositionalRulesRSRCID = 17732;
		kPropSymbolizationRulesRSRCID = 13351;
		kRulesRSRCID = 335;
		kTransitive_VerbsRSRCID = 20575;
		kVariablesRSRCID = 12892;

		kPropAnalysis = false;
		kPredAnalysis = true;


	type
		listtokentype = (adj, binAdj, engWord, name, noun, qR, qU, iVerb, pVerb, tVerb, both, either, english_connective, english_iff, english_neg, i_f, neither, the_n, proposition, unknowntoken, endlist);



	procedure InitEtoLUnit; (*this must be done after gNil is initialized in InitLisp*)



	procedure SymbolizeOneStep (predicate: boolean; english_word_list: integer; var success: boolean);

	function TranslateBack (thisformula: TFormula; aParser: TParser): boolean;

	procedure WriteUnaryAssocList (list: integer);
	procedure WriteBinaryAssocList (list: integer);


	function SentenceParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;
	procedure getpredlisttoken (var from_this_list: integer; var token: integer; var tokentype: listtokentype);
	function sentence_p (word_list: integer): integer;

implementation


{$IFC myDebugging}






	procedure writetokentype (token: integer; ttype: listtokentype);
	forward;



{$ENDC}
	function SubjParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;
	forward;



{$S EnglishToLogic}



	function apply_rule_matching_function (rule, list_of_arguments: integer): integer;
	forward;

(*******assoc list *********)

	function add_special_binding (variable_expr, datum, symbolization, bindings: integer): integer;
	forward;
	function make_special_binding (variable, datum, symbolization: integer): integer;
	forward;
	function rule_if (rule: integer): integer;
	forward;
	function rule_matching_key (rule: integer): string;
	forward;
	function rule_name (rule: integer): integer;
	forward;
	function rule_then (rule: integer): integer;
	forward;

(**********matching **********)

	function general_match (word_list, variable_pair_list: integer): integer;
	forward;
	function left_literal_then_middle_match (word_list, variable_pair_list: integer): integer;
	forward;
	function left_literal_then_middle_match_aux (target, left_literal, left_variable_pair, middle_variable_pair, right_variable_pair: integer): integer;
	forward;
	function left_match (word_list, variable_pair_list: integer): integer;
	forward;
	function left_match_aux (target, left_literal, variable, test: integer): integer;
	forward;
	function middle_match (word_list, variable_pair_list: integer): integer;
	forward;
	function middle_match_aux (target, left_variable_pair, middle_variable_pair, right_variable_pair: integer): integer;
	forward;
	function total_match (word_list, variable_pair_list: integer): integer;
	forward;














	function proposition_p (english_word_list: integer): integer;
	forward;



	function try_rule (rule, assertion: integer): integer;
	forward;

	function instantiate_variables (pattern, a_list: integer): integer;
	forward;

	function try_all_the_rules (assertion: integer; var english: boolean): integer;
	forward;

	function try_all_the_propositional_rules (assertion: integer; var english: boolean): integer;
	forward;





(*    function identity:function; *)

	function add_binding_to_each_member_of_list (variable_expr, datum, symbolization, list_of_assoc_lists: integer): integer;
	forward;

	function proposition_parse (thislist: integer): boolean;
	forward;

	function adj_p (word_list: integer): integer;
	forward;
	function adjnoun_p (word_list: integer): integer;
	forward;
	function atomic_proposition_p (english_word_list: integer): integer;
	forward;
	function badj_p (word_list: integer): integer;
	forward;
	function bp_p (word_list: integer): integer;
	forward;
	function dp_p (word_list: integer): integer;
	forward;
	function noun_p (word_list: integer): integer;
	forward;
	function np_p (word_list: integer): integer;
	forward;
	function passv_p (word_list: integer): integer;
	forward;
	function rv_p (word_list: integer): integer;
	forward;

	function subject_p (word_list: integer): integer;
	forward;
	function term_p (word_list: integer): integer;
	forward;
	function tvp_p (word_list: integer): integer;
	forward;
	function vip_p (word_list: integer): integer;
	forward;
	function vp_p (word_list: integer): integer;
	forward;



	procedure InitEtoLUnit;
		var
			dummy: integer;

		procedure ReadGrammar;
		begin
			InitializeResourceAssociationList(gAdjectives, kAdjectivesRSRCID);  (*reads from Text in resource*)
			InitializeResourceAssociationList(gBinaryAdjs, kBinaryAdjsRSRCID);  (*reads from Text in resource*)
			InitializeResourceAssociationList(gNames, kNamesRSRCID);  (*reads from Text in resource*)
{InitializeResourceAssociationList(gRules, kRulesRSRCID);   kludge}
			InitializeResourceAssociationList(gPassiveVerbs, kPassiveVerbsRSRCID);  (*reads from Text in resource*)
			InitializeResourceAssociationList(gPropSymbolizationRules, kPropSymbolizationRulesRSRCID);  (*reads from Text in resource*)
{InitializeResourceAssociationList(gPredSymbolizationRules, kPredSymbolizationRulesRSRCID);  kludge}
			InitializeResourceAssociationList(gPropositions, kPropsRSRCID);  (*reads from Text in resource*)
			InitializeResourceAssociationList(gPropositionalRules, kPropositionalRulesRSRCID);
			InitializeResourceAssociationList(gNouns, kNounsRSRCID);  (*reads from Text in resource*)
			InitializeResourceAssociationList(gIntransitive_Verbs, kIntransitive_VerbsRSRCID);  (*reads from Text in resource*)
			InitializeResourceAssociationList(gTransitive_Verbs, kTransitive_VerbsRSRCID);  (*reads from Text in resource*)
			InitializeResourceAssociationList(gEngVariables, kVariablesRSRCID);  (*reads from Text in resource*)

		end;

		procedure FormEngkeywords;
		begin
			gEngkeywords := gNil;
			gEngkeywords := cons(InternalStringtoSExpression('IT'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('IS'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('NOT'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('THE'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('CASE'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('THAT'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('BOTH'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('AND'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('EITHER'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('OR'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('NEITHER'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('NOR'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('IF'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('THEN'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('ONLY'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('UNLESS'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('ITSELF'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('HIMSELF'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('HERSELF'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('A'), gEngkeywords);
			gEngkeywords := cons(InternalStringtoSExpression('DOES'), gEngkeywords);
		end;
		procedure FormUquantWords;
		begin
			gUquantwords := gNil;
			gUquantwords := cons(InternalStringtoSExpression('EVERYTHING'), gUquantwords);
			gUquantwords := cons(InternalStringtoSExpression('SOMETHING'), gUquantwords);
			gUquantwords := cons(InternalStringtoSExpression('NOTHING'), gUquantwords);
		end;
		procedure FormRquantWords;
		begin
			gRquantwords := gNil;
			gRquantwords := cons(InternalStringtoSExpression('EVERY'), gRquantwords);
			gRquantwords := cons(InternalStringtoSExpression('SOME'), gRquantwords);
			gRquantwords := cons(InternalStringtoSExpression('NO'), gRquantwords);
		end;


	begin
		InitializeLispUnit;

		gRules := gNil;
		gPropSymbolizationRules := gNil;
		gPredSymbolizationRules := gNil;
		gNames := gNil;
		gAdjectives := gNil;
		gBinaryAdjs := gNil;
		gPassiveVerbs := gNil;
		gPropositions := gNil;
		gPropositionalRules := gNil;
		gNouns := gNil;
		gIntransitive_Verbs := gNil;
		gTransitive_Verbs := gNil;
		gEngVariables := gNil;


		readgrammar;

		FormEngkeywords;
		FormUQuantWords;
		FormRQuantWords;



	end;

	function fundispatch (function_key, argument: integer): integer;
		var
			key: string;
			literal: integer;
	begin
		key := svalue(first(function_key));  (*used to be furst*)
{$IFC myDebugging}
(*    writeln('fundispatch', key); *)
(*    writeln('argument '); *)
(*    diagnose(argument); *)


{$ENDC}
		if (key = 'ADJP') then
			fundispatch := adj_p(argument)
		else if (key = 'ADJNOUNP') then
			fundispatch := adjnoun_p(argument)
		else if (key = 'BADJP') then
			fundispatch := badj_p(argument)
		else if (key = 'BPP') then
			fundispatch := bp_p(argument)
		else if (key = 'DPP') then
			fundispatch := dp_p(argument)
		else if (key = 'NOUNP') then
			fundispatch := noun_p(argument)
		else if (key = 'NP') then
			fundispatch := np_p(argument)
		else if (key = 'PASSVP') then
			fundispatch := passv_p(argument)
		else if (key = 'PP') then
			fundispatch := proposition_p(argument)
		else if (key = 'RVP') then
			fundispatch := rv_p(argument)
		else if (key = 'SP') then
			fundispatch := sentence_p(argument)
		else if (key = 'SUP') then
			fundispatch := subject_p(argument)
		else if (key = 'TERMP') then
			fundispatch := term_p(argument)
		else if (key = 'VIP') then
			fundispatch := vip_p(argument)
		else if (key = 'VP') then
			fundispatch := vp_p(argument)
		else if (key = 'VTP') then
			fundispatch := tvp_p(argument)
		else if (key = 'EQUAL') then
			begin
				if equal(second(function_key), argument) then
					fundispatch := gT
				else
					fundispatch := gNil
			end
		else
			fundispatch := gNil;
	end;




(*********Rule Accessors************)

(*Rules are quads: match key, if, then, name*)
(*the rule if is a list of pairs of variables and tests*)
(*The name is there for debugging purposes. The actual running*)
(*version will consist of triples, and the name will never be*)
(*accessed*)

	function rule_matching_key (rule: integer): string;
	begin
		rule_matching_key := svalue(first(rule));
	end;

	function rule_if (rule: integer): integer;
	begin
		rule_if := first(cdr(rule));
	end;

	function rule_then (rule: integer): integer;
	begin
		rule_then := first(cdr(cdr(rule)));
	end;

	function rule_name (rule: integer): integer;
	begin
		rule_name := first(cdr(cdr(cdr(rule))));
	end;

(************ function dispatchers           **************)

	function apply_rule_matching_function (rule, list_of_arguments: integer): integer;

(*a dispatcher*)
		var
			function_key: string;
	begin
		function_key := rule_matching_key(rule);


{$IFC myDebugging}

		if FALSE then
			begin
				writeln('matching dispatch', function_key);

				writeln('with rule');

				diagnose(rule_name(rule));
			end;

{$ENDC}

		if function_key = 'TM' then
			apply_rule_matching_function := total_match(first(list_of_arguments), second(list_of_arguments))
		else if function_key = 'LM' then
			apply_rule_matching_function := left_match(first(list_of_arguments), second(list_of_arguments))
		else if function_key = 'MM' then
			apply_rule_matching_function := middle_match(first(list_of_arguments), second(list_of_arguments))
		else if function_key = 'LTMM' then
			apply_rule_matching_function := left_literal_then_middle_match(first(list_of_arguments), second(list_of_arguments))
		else if function_key = 'GM' then
			apply_rule_matching_function := general_match(first(list_of_arguments), second(list_of_arguments))

		else
			apply_rule_matching_function := gNil;

	end;


(************* manipulating if-lists *************)

(*this are lists of pairs of variables ans tests *)

	function extract_var (var_pair: integer): integer;
	begin
		extract_var := car(var_pair);
	end;


	function extract_test (var_pair: integer): integer;
	begin
		extract_test := second(var_pair);
	end;

{* * * * * * * * * * * * association list and its entries}
{the association listfor bindings will typically contain three element lists .;}
{First there will be a variable A , say .Then there will be the list of English words that the variable;}
{A has been matched against ( ornithologist ) , say . Finally there}
{will be the logical symbolization of that English expression O, say.Sot }
{he association list might look like ( ( A ( ornithologist ) O )}
{( B ( ornithologist and expert ) O & E );}
{}
{and from this we can read , that for example , the symbolization of;}
{whatever the variable B is matched against is O & E .}


	function add_special_binding (variable_expr, datum, symbolization, bindings: integer): integer;
		var
			variable: integer;
	begin
		variable := extract_variable(variable_expr);
		if svalue(variable) = '_' then      (*don't add anonymous*)
			add_special_binding := bindings
		else
			add_special_binding := cons(make_special_binding(variable, datum, symbolization), bindings);
	end;


	function make_special_binding (variable, datum, symbolization: integer): integer;
	begin
		make_special_binding := cons(variable, cons(datum, cons(symbolization, gNil)));
	end;

	function extract_symbolization (binding: integer): integer;
	begin
		extract_symbolization := third(binding);
	end;





(************matching **************)

(* * * * * * * * * * * * * General Match *)

(*We are looking for a very general matching procedure*)
(*    that will operate between two lists . The first list *)
(* will typically be a list of words . The second list; }
{will be a list of variables and}
{with each variable;}
{there will be a test . These variables are going to be;}
{matched against 'sublists' of the first list satisfying;}

{the test . And the algorithm should generate all matches .;}
{;}
{For example , ( The cat ) matched}
{with ( ( ( ?}
{A ) ( test = 'The)) ((? B)(test}
{;}
{;}
{;}
{identity}
{)}
{)}
{)}
{should give ( ?}
{A ) The and ( ?}
{B ) Cat *)


(*NOTE this has been rewritten so as to match from the right to the left*)
(*This avoids so manny calls to butlast (which copies a list*)
(*and does surgery.*)

	function general_match (word_list, variable_pair_list: integer): integer;
		var
			result_list, target_length, no_of_variables, test, count, symbolization: integer;
			head_assoc_list: integer;
	begin
		result_list := gNil;
		target_length := listlength(word_list);
		no_of_variables := listlength(variable_pair_list);

		if endp(word_list) & endp(variable_pair_list) then
			general_match := gT
		else if endp(word_list) | endp(variable_pair_list) then
			general_match := gNil
		else
			begin
				test := extract_test(lastelement(variable_pair_list));

(*one variable is a special case-- it must match the whole list*)
				if no_of_variables = 1 then
					begin
						symbolization := fundispatch(test, word_list);
						if (symbolization <> gNil) then
							result_list := append(result_list, add_binding_to_each_member_of_list(extract_var(lastelement(variable_pair_list)), word_list, symbolization, gNil));
					end
				else
					begin
						for count := 0 to (target_length - no_of_variables) do (*off by one error here*)
							begin
								symbolization := fundispatch(test, nthcdr(target_length - (count + 1), word_list));
								if (symbolization <> gNil) then
									begin
										head_assoc_list := general_match(butlast(word_list, (count + 1)), butlast(variable_pair_list, 1));
										if (head_assoc_list <> gNil) then
											result_list := append(result_list, add_binding_to_each_member_of_list(extract_var(lastelement(variable_pair_list)), nthcdr((target_length - (count + 1)), word_list), symbolization, head_assoc_list));
									end;
							end;
					end;
				general_match := result_list;
			end;
	end;


	function left_literal_then_middle_match (word_list, variable_pair_list: integer): integer;
	begin
		left_literal_then_middle_match := left_literal_then_middle_match_aux(word_list, extract_test(first(variable_pair_list)), second(variable_pair_list), third(variable_pair_list), fourth(variable_pair_list));
	end;

	function left_literal_then_middle_match_aux (target, left_literal, left_variable_pair, middle_variable_pair, right_variable_pair: integer): integer;
	begin
		if endp(left_literal) then
			left_literal_then_middle_match_aux := middle_match_aux(target, left_variable_pair, middle_variable_pair, right_variable_pair)
		else
			begin
				if equal(first(left_literal), first(target)) then
					left_literal_then_middle_match_aux := left_literal_then_middle_match_aux(cdr(target), cdr(left_literal), left_variable_pair, middle_variable_pair, right_variable_pair)
				else
					left_literal_then_middle_match_aux := gNil;
			end;
	end;



	function left_match (word_list, variable_pair_list: integer): integer;
		var
			symbolization, left_literal, variable, test: integer;
	begin
		left_literal := extract_test(first(variable_pair_list));
		variable := extract_var(second(variable_pair_list));
		test := extract_test(second(variable_pair_list));

(*the test is a lambda expr in lisp but here it is one element key*)

		left_match := left_match_aux(word_list, left_literal, variable, test);
	end;

	function left_match_aux (target, left_literal, variable, test: integer): integer;
		var
			symbolization: integer;
	begin
		if target = gNil then
			left_match_aux := gNil
		else
			begin
				if endp(left_literal) then
					begin
						symbolization := fundispatch(test, target);
						if (symbolization = gNil) then
							left_match_aux := gNil
						else
							left_match_aux := cons(add_special_binding(variable, target, symbolization, gNIL), gNIl);
					end
				else if equal(first(left_literal), first(target)) then
					left_match_aux := left_match_aux(cdr(target), cdr(left_literal), variable, test)
				else
					left_match_aux := gNil
			end;
	end;

	function middle_match (word_list, variable_pair_list: integer): integer;
	begin
		middle_match := middle_match_aux(word_list, first(variable_pair_list), second(variable_pair_list), third(variable_pair_list));
	end;


	function middle_match_aux (target, left_variable_pair, middle_variable_pair, right_variable_pair: integer): integer;
		var
			left_variable, test1, middle_literal, right_variable, test2, length_target, length_middle: integer;
			count: integer;
			temp_left, temp_middle, temp_right: integer;
			startindex, index: integer;
			result_list: integer;
			left_symbolization, right_symbolization: integer;

	begin
		result_list := gNil;
		left_variable := extract_var(left_variable_pair);
		test1 := extract_test(left_variable_pair);
		middle_literal := extract_test(middle_variable_pair);


		right_variable := extract_var(right_variable_pair);

		test2 := extract_test(right_variable_pair);
		length_target := listlength(target);
		length_middle := listlength(middle_literal);

		if (length_target < 3) | (length_target < (length_middle + 2)) then
			middle_match_aux := gNil
		else
			begin
				startindex := 0;
				while find_index(target, middle_literal, startindex, index) do
					begin
						startindex := index + 1;
						temp_left := target;
						split_into3(temp_left, temp_middle, temp_right, index, (index + length_middle));

						left_symbolization := fundispatch(test1, temp_left);

						if not (left_symbolization = gNil) then
							begin
								right_symbolization := fundispatch(test2, temp_right);

								if not (right_symbolization = gNil) then
									result_list := cons(add_special_binding(right_variable, temp_right, right_symbolization, add_special_binding(left_variable, temp_left, left_symbolization, gNIL)), result_list);
							end;
					end;
			end;
		middle_match_aux := result_list;
	end;






	function total_match (word_list, variable_pair_list: integer): integer;
		var
			symbolization: integer;
	begin
		symbolization := proposition_p(word_list);
		if symbolization = gNil then
			total_match := gNIl
		else
			total_match := cons(add_special_binding(extract_var(first(variable_pair_list)), word_list, symbolization, gNIL), gNIl);
	end;





(************ list parser ******************)

{$IFC myDebugging}

	procedure writetokentype (token: integer; ttype: listtokentype);
	begin
		writeln('token ', svalue(token));

		if ttype = engword then
			writeln('englword')
		else if ttype = qU then
			writeln('uquant')
		else if ttype = qR then
			writeln('rquant')
		else if ttype = iVerb then
			writeln('iverb')
		else if ttype = noun then
			writeln('noun')
		else if ttype = adj then
			writeln('adj')
		else if ttype = tverb then
			writeln('tverb')
		else if ttype = binadj then
			writeln('binadj')
		else if ttype = pverb then
			writeln('pverb')
		else if ttype = name then
			writeln('name')
	end;

{$ENDC}

(*predicate case*)

	procedure getpredlisttoken (var from_this_list: integer; var token: integer; var tokentype: listtokentype);
{note that some items like binary adjectives take two atoms, so their type is right}
{but the token gets only the first one. Unfortunately binary adjectives can take one atom or}
{two, which makes it even more complicated}

		var
			lengthoflist, key: integer;
	begin
		tokentype := unknowntoken;
		token := gNil;

		lengthoflist := listlength(from_this_list);

		if (lengthoflist = 0) then
			begin
				token := gNil;
				tokentype := endlist;
			end
		else
			begin
				if (listmember(car(from_this_list), gEngkeywords) <> gNil) then
					begin
						tokentype := engword;
						token := car(from_this_list);
						from_this_list := cdr(from_this_list);
					end
				else if (listmember(car(from_this_list), gUquantwords) <> gNil) then
					begin
						tokentype := qU;
						token := car(from_this_list);
						from_this_list := cdr(from_this_list);
					end
				else if (listmember(car(from_this_list), gRquantwords) <> gNil) then
					begin
						tokentype := qR;
						token := car(from_this_list);
						from_this_list := cdr(from_this_list);
					end
				else
					begin
						key := cons(car(from_this_list), gNil);

						if (assoc(key, gIntransitive_Verbs) <> gNil) then
							begin
								tokentype := iVerb;
								token := car(from_this_list);
								from_this_list := cdr(from_this_list);
							end
						else if (assoc(key, gNouns) <> gNil) then
							begin
								tokentype := Noun;
								token := car(from_this_list);
								from_this_list := cdr(from_this_list);
							end
						else if (assoc(key, gAdjectives) <> gNil) then
							begin
								tokentype := adj;
								token := car(from_this_list);
								from_this_list := cdr(from_this_list);
							end
						else if (assoc(key, gTransitive_Verbs) <> gNil) then
							begin
								tokentype := tverb;
								token := car(from_this_list);
								from_this_list := cdr(from_this_list);
							end
						else if (assoc(key, gNames) <> gNil) | (assoc(key, gEngVariables) <> gNil) then
							begin
								tokentype := name;
								token := car(from_this_list);
								from_this_list := cdr(from_this_list);
							end
						else if (assoc(key, gBinaryAdjs) <> gNil) then
							begin
								tokentype := binAdj;
								token := car(from_this_list);
								from_this_list := cdr(from_this_list);
							end
						else
							begin
								if (lengthoflist > 1) then
									begin
										key := cons(car(from_this_list), cons(car(cdr(from_this_list)), gNil));

										if (assoc(key, gPassiveVerbs) <> gNil) then
											begin
												tokentype := pVerb;
												token := car(from_this_list);
												from_this_list := cdr(cdr(from_this_list));
											end
										else if (assoc(key, gBinaryAdjs) <> gNil) then
											begin
												tokentype := binAdj;
												token := car(from_this_list);
												from_this_list := cdr(cdr(from_this_list));
											end
									end;
							end;
					end;
			end;
	end;



(*propositional case*)

	procedure getlisttoken (var from_this_list: integer; var token: integer; var tokentype: listtokentype);
		var
			key: string;
			found: boolean;
			lengthoflist, n, prop_value: integer;
	begin
		tokentype := unknowntoken;
		token := gNil;

		lengthoflist := listlength(from_this_list);

		if (lengthoflist = 0) then
			begin
				token := gNil;
				tokentype := endlist;
			end
		else if (lengthoflist = 1) then                             (*no single word propositions*)
			begin
				tokentype := unknowntoken;
				token := gNil;
			end
		else
			begin
				key := kNull;
				key := svalue(first(from_this_list));

				if (key = 'BOTH') then
					begin
						tokentype := both;
						token := car(from_this_list);
						from_this_list := cdr(from_this_list);
					end
				else if (key = 'EITHER') then
					begin
						tokentype := either;
						token := car(from_this_list);
						from_this_list := cdr(from_this_list);
					end
				else if (key = 'NEITHER') then
					begin
						tokentype := neither;
						token := car(from_this_list);
						from_this_list := cdr(from_this_list);
					end
				else if ((key = 'IF') & (svalue(second(from_this_list)) <> 'AND')) then
					begin
						tokentype := i_f;
						token := car(from_this_list);
						from_this_list := cdr(from_this_list);
					end
				else if (key = 'THEN') then
					begin
						tokentype := the_n;
						token := car(from_this_list);
						from_this_list := cdr(from_this_list);
					end
				else if (key = 'BOTH') | (key = 'AND') | (key = 'EITHER') | (key = 'OR') | (key = 'NEITHER') | (key = 'NOR') | ((key = 'IF') & (svalue(second(from_this_list)) <> 'AND')) | (key = 'UNLESS') then
					begin
						tokentype := english_connective;
						token := car(from_this_list);
						from_this_list := cdr(from_this_list);
					end
				else if (key = 'ONLY') & (svalue(second(from_this_list)) = 'IF') then
					begin
						tokentype := english_connective;
						token := car(from_this_list);
						from_this_list := cdr(cdr(from_this_list));
					end
				else if (lengthoflist > 5) & (key = 'IF') & (svalue(second(from_this_list)) = 'AND') & (svalue(third(from_this_list)) = 'ONLY') & (svalue(fourth(from_this_list)) = 'IF') then
					begin
						tokentype := english_connective;
						token := gNil;
						from_this_list := nthcdr(4, from_this_list);
					end
				else if (lengthoflist > 7) & (key = 'IT') & (svalue(second(from_this_list)) = 'IS') & (svalue(third(from_this_list)) = 'NOT') & (svalue(fourth(from_this_list)) = 'THE') & (svalue(fifth(from_this_list)) = 'CASE') & (svalue(sixth(from_this_list)) = 'THAT') then
					begin
						tokentype := english_neg;
						token := gNil;
						from_this_list := nthcdr(6, from_this_list);
					end
				else
					begin
						found := false;  (*looking for atomic proposition*)
						n := 2;
						while (n <= lengthoflist) & not found do (*no single word props*)
							begin
								prop_value := atomic_proposition_p(butlast(from_this_list, (lengthoflist - n)));

								if (prop_value <> gNil) then
									begin
										found := true;
										tokentype := proposition;
										token := prop_value;
										from_this_list := nthcdr(n, from_this_list);
									end
								else
									n := n + 1;
							end;
					end;
			end;
	end;

{Each of these routines expects to be fed the first token and tokentype and leaves with}
{the token and tokentype looking at the next one. Notice that they do not check for}
{illegimate extra stuff on the end, the calling routine has to do this.}

{NOTICE that there is a problem with the FOLLOWS.  For example, 'and' which }
{might appear in 'Arthur goes and Arthur faints' and in 'Arthur goes and faints'}
{now if you are parsing goes and lookahead to 'and' you don't know at that}
{stage what to do.  My kludge is to write VPParse to consume the max}
{possible so the verb tries to be 'goes and <something>' and if it fails it}
{backtracks to be just 'goes'. }


	function NPParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;
	forward;
	function RelClauseParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;
	forward;
	function RVParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;
	forward;
	function VPParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;
	forward;

	function BPParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;

(* <top>: *)
(*             <tertiary> ok*)
(*             |<tertiary> and <top> ok*)
(*             |<tertiary> or <top> ok*)
(*<tertiary>:*)
(*             | both <tertiary> and <top> ok*)
(*             | either <tertiary> or <top> ok*)
(*             | neither <top> nor <top> ok*)
(*             | a <NP>*)
(*             | pverb <subj>*)
(*             | binadj <subj>*)
(*             | pverb itself/himself/herself *)
(*             | binadj itself/himself/herself  *)
(*             | adj*)


		function top: boolean;
		forward;

		function tertiary: boolean;
		begin
			tertiary := false;

			case tokentype of
				engword: 
					if (svalue(token) = 'BOTH') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'AND') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end
					else if (svalue(token) = 'EITHER') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'OR') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end
					else if (svalue(token) = 'NEITHER') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'NOR') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end
					else if (svalue(token) = 'A') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							tertiary := NPParse(thislist, token, tokentype);
						end;


				pverb, binadj: 
					begin
						getpredlisttoken(thislist, token, tokentype);   (*alters thislist locally by advancing*)
						if (tokentype = engword) & ((svalue(token) = 'ITSELF') | (svalue(token) = 'HIMSELF') | (svalue(token) = 'HERSELF')) then
							begin
								tertiary := true;
								getpredlisttoken(thislist, token, tokentype);
							end
						else
							tertiary := SubjParse(thislist, token, tokentype);
					end;

				adj: 
					begin
						tertiary := true;
						getpredlisttoken(thislist, token, tokentype);
					end;


				otherwise
			end;
		end;



		function top: boolean;
		begin
			top := false;

			if tertiary then
				begin
					if (tokentype = engword) & ((svalue(token) = 'AND') | (svalue(token) = 'OR')) then
						begin
							getpredlisttoken(thislist, token, tokentype);
							top := top;
						end
					else
						top := true;
				end;
		end;

	begin
		BPParse := top;
	end;

	function DPParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;

(* <top>: *)
(*             <tertiary> ok*)
(*             |<tertiary> and <top> ok*)
(*             |<tertiary> or <top> ok*)
(*<tertiary>:*)
(*             | both <tertiary> and <top> ok*)
(*             | either <tertiary> or <top> ok*)
(*             | neither <top> nor <top> ok*)
(*             | iverb *)
(*             | tverb <subj>*)
(*             | tverb itself/HIM/HER*)


		function top: boolean;
		forward;

		function tertiary: boolean;
		begin
			tertiary := false;

			case tokentype of
				engword: 
					if (svalue(token) = 'BOTH') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'AND') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end
					else if (svalue(token) = 'EITHER') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'OR') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end
					else if (svalue(token) = 'NEITHER') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'NOR') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end;


				iverb: 
					begin
						tertiary := true;
						getpredlisttoken(thislist, token, tokentype);   (*alters thislist locally by advancing*)
					end;

				tverb: 
					begin
						getpredlisttoken(thislist, token, tokentype);   (*alters thislist locally by advancing*)
						if (tokentype = engword) & ((svalue(token) = 'ITSELF') | (svalue(token) = 'HIMSELF') | (svalue(token) = 'HERSELF')) then
							begin
								tertiary := true;
								getpredlisttoken(thislist, token, tokentype);
							end
						else
							tertiary := SubjParse(thislist, token, tokentype);
					end;


				otherwise
			end;
		end;



		function top: boolean;
		begin
			top := false;

			if tertiary then
				begin
					if (tokentype = engword) & ((svalue(token) = 'AND') | (svalue(token) = 'OR')) then
						begin
							getpredlisttoken(thislist, token, tokentype);
							top := top;
						end
					else
						top := true;
				end;
		end;

	begin
		DPParse := top;
	end;


	function NPParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;

(* <NP>: *)
(*             (0 to n adj>) noun (0 or 1 RelClause ok*)

	begin
		NPParse := false;

		while (tokentype = adj) do
			getpredlisttoken(thislist, token, tokentype);

		if (tokentype = noun) then
			begin
				getpredlisttoken(thislist, token, tokentype);

				if (tokentype = engword) & (svalue(token) = 'THAT') then
					NPParse := RelClauseParse(thislist, token, tokentype)
				else
					NPParse := true;
			end;
	end;


	function RelClauseParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;

		var
			altToken, altList: integer;
			altTokentype: listtokentype;

(* <RelClause>: *)
(*             that <VP> *)
(*             that <Subj> <RV> *)


	begin
		RelClauseParse := false;

		if (tokentype = engword) & (svalue(token) = 'THAT') then
			begin
				getpredlisttoken(thislist, token, tokentype);

				altToken := token;
				altList := thislist;
				altTokentype := tokentype;

				if VPParse(thislist, token, tokentype) then
					RelClauseParse := true
				else
					begin
						if SubjParse(altList, altToken, altTokentype) then
							RelClauseParse := RVParse(altList, altToken, altTokentype);
					end;

			end;
	end;

	function RVParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;

(* <RV>: *)
(*             tverb*)
(*      is       pverb*)
(*      is       binadj*)

	begin
		RVParse := false;

		if (tokentype = engword) & (svalue(token) = 'IS') then
			begin
				getpredlisttoken(thislist, token, tokentype);
				if ((tokentype = pverb) | (tokentype = binadj)) then
					begin
						RVParse := true;
						getpredlisttoken(thislist, token, tokentype);
					end
			end
		else if (tokentype = tverb) then
			begin
				RVParse := true;
				getpredlisttoken(thislist, token, tokentype);
			end;
	end;

	function SentenceParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;

(*this can be awkward as both subj and sentence can begin with 'both', 'either', or 'neither'*)
(*we try for both*)

(* <top>: *)
(*             <tertiary> ok*)
(*             |<tertiary> and <top> ok*)
(*             |<tertiary> or <top> ok*)
(*             |<tertiary> if <top> ok*)
(*             |<tertiary> if and only if <top> ok*)
(*             |<tertiary> unless <top> ok*)
(*             |<tertiary> only if <top> ok*)

(*<tertiary>:*)
{NOTE THE NEXT THREE MAY BE SUBJECTS NOT SENTENCES}
(*             | both <tertiary> and <top> ok*)
(*             | either <tertiary> or <top> ok*)
(*             | neither <top> nor <top> ok*)
(*             | if <top> then <top> ok*)
(*             | it is not the case that <top>ok*)
(*             | <secondary>*)

(*<secondary>:*)
(*             | <subj> <NP>*)


		function top: boolean;
		forward;

		function secondary: boolean;
		begin
			secondary := false;
			if SubjParse(thislist, token, tokentype) then
				if VPParse(thislist, token, tokentype) then
					secondary := true;
		end;


		function tertiary: boolean;
			var
				altToken, altList: integer;
				altTokenType: listtokentype;
				temp, dummy: boolean;
		begin
			temp := false;
			altToken := token;
			altList := thislist;
			altTokenType := tokentype;

			case tokentype of

(*Theres a problem with both, either, and neither which can be the first of sentences*)
(*and the first of subjects.  The kludge here is to try for sentences first, and if that*)
(*fails backtrack to try subjects*)

				engword: 
					if (svalue(token) = 'BOTH') then
						begin
(*we try to advance as sentences using alternatives*)

							getpredlisttoken(altList, altToken, altTokentype);
							if SentenceParse(altList, altToken, altTokentype) then
								begin
									if (altTokentype = engword) & (svalue(altToken) = 'AND') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											if SentenceParse(altList, altToken, altTokentype) then
												begin
													temp := true;
													getpredlisttoken(thislist, token, tokentype); (*pass both*)
													dummy := tertiary;
													getpredlisttoken(thislist, token, tokentype); (*pass and*)
													dummy := top;
												end;
										end
								end;

(*if we cannot, we try to advance as compound subjects*)
							if not temp then
								temp := secondary;

						end
					else if (svalue(token) = 'EITHER') then
						begin
(*we try to advance as sentences using alternatives*)

							getpredlisttoken(altList, altToken, altTokentype);
							if SentenceParse(altList, altToken, altTokentype) then
								begin
									if (altTokentype = engword) & (svalue(altToken) = 'OR') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											if SentenceParse(altList, altToken, altTokentype) then
												begin
													temp := true;
													getpredlisttoken(thislist, token, tokentype); (*pass EITHER*)
													dummy := tertiary;
													getpredlisttoken(thislist, token, tokentype); (*pass OR*)
													dummy := top;
												end;
										end
								end;

(*if we cannot, we try to advance as compound subjects*)
							if not temp then
								temp := secondary;

						end
					else if (svalue(token) = 'NEITHER') then
						begin
(*we try to advance as sentences using alternatives*)

							getpredlisttoken(altList, altToken, altTokentype);
							if SentenceParse(altList, altToken, altTokentype) then
								begin
									if (altTokentype = engword) & (svalue(altToken) = 'NOR') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											if SentenceParse(altList, altToken, altTokentype) then
												begin
													temp := true;
													getpredlisttoken(thislist, token, tokentype); (*pass NEITHER*)
													dummy := tertiary;
													getpredlisttoken(thislist, token, tokentype); (*pass NOR*)
													dummy := top;
												end;
										end
								end;

(*if we cannot, we try to advance as compound subjects*)
							if not temp then
								temp := secondary;

						end
					else if (svalue(token) = 'IF') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'THEN') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											temp := top;
										end
								end;
						end
					else if (svalue(token) = 'IT') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if (tokentype = engword) & (svalue(token) = 'IS') then
								begin
									getpredlisttoken(thislist, token, tokentype);
									if (tokentype = engword) & (svalue(token) = 'NOT') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											if (tokentype = engword) & (svalue(token) = 'THE') then
												begin
													getpredlisttoken(thislist, token, tokentype);
													if (tokentype = engword) & (svalue(token) = 'CASE') then
														begin
															getpredlisttoken(thislist, token, tokentype);
															if (tokentype = engword) & (svalue(token) = 'THAT') then
																begin
																	getpredlisttoken(thislist, token, tokentype);
																	temp := top;
																end
															else
																temp := false;
														end
													else
														temp := false;
												end
											else
												temp := false;
										end
									else
										temp := false;
								end
							else
								temp := false;
						end
					else
						temp := false;

				otherwise
					temp := secondary;
			end;

			tertiary := temp;
		end;




		function top: boolean;
		begin
			top := false;

			if tertiary then
				begin
					if (tokentype = engword) & ((svalue(token) = 'AND') | (svalue(token) = 'OR') | (svalue(token) = 'UNLESS')) then
						begin
							getpredlisttoken(thislist, token, tokentype);
							top := top;
						end

(******* IF and IF AND ONLY IF ********)
					else if (tokentype = engword) & (svalue(token) = 'IF') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if (tokentype = engword) & (svalue(token) = 'AND') then
								begin
									getpredlisttoken(thislist, token, tokentype);
									if (tokentype = engword) & (svalue(token) = 'ONLY') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											if (tokentype = engword) & (svalue(token) = 'IF') then
												begin
													getpredlisttoken(thislist, token, tokentype);
													top := top;
												end
											else
												top := false;
										end
									else
										top := false;
								end
							else
								top := top;
						end

(*******  end IF and IF AND ONLY IF ********)

(*******  ONLY IF ********)
					else if (tokentype = engword) & (svalue(token) = 'ONLY') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if (tokentype = engword) & (svalue(token) = 'IF') then
								begin
									getpredlisttoken(thislist, token, tokentype);
									top := top;
								end
							else
								top := false;
						end
(*******  end ONLY IF ********)

					else

(****** straght tertiary ******)
						top := true;
				end;
		end;

	begin
		SentenceParse := top;
	end;

	function SubjParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;


(*  NOTE I have omitted <tertiary>           | the <NP> for the present as it involves the*)
(*iota operator which I am not bothering with*)


(* <top>: *)
(*             <tertiary> ok*)
(*             |<tertiary> and <top> ok*)
(*             |<tertiary> or <top> ok*)
(*<tertiary>:*)
(*             | both <tertiary> and <top> ok*)
(*             | either <tertiary> or <top> ok*)
(*             | neither <top> nor <top> ok*)
(*             | name ok*)
(*             | qU ok*)
(*             | qU <RelClause> ok*)
(*             | qR <NP> ok*)


		function top: boolean;
		forward;

		function tertiary: boolean;
		begin
			tertiary := false;

			case tokentype of
				engword: 
					if (svalue(token) = 'BOTH') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'AND') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end
					else if (svalue(token) = 'EITHER') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'OR') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end
					else if (svalue(token) = 'NEITHER') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'NOR') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end;


				name: 
					begin
						tertiary := true;
						getpredlisttoken(thislist, token, tokentype);   (*alters thislist locally by advancing*)
					end;

				qU: 
					begin
						getpredlisttoken(thislist, token, tokentype);   (*alters thislist locally by advancing*)
						if (tokentype = engword) & (svalue(token) = 'THAT') then
							tertiary := RelClauseParse(thislist, token, tokentype)
						else
							tertiary := true;
					end;

				qR: 
					begin
						getpredlisttoken(thislist, token, tokentype);   (*alters thislist locally by advancing*)
						tertiary := NPParse(thislist, token, tokentype);
					end;

				otherwise
			end;
		end;



		function top: boolean;
		begin
			top := false;

			if tertiary then
				begin
					if (tokentype = engword) & ((svalue(token) = 'AND') | (svalue(token) = 'OR')) then
						begin
							getpredlisttoken(thislist, token, tokentype);
							top := top;
						end
					else
						top := true;
				end;
		end;

	begin
		SubjParse := top;
	end;

	function VPParse (var thislist: integer; var token: integer; var tokentype: listtokentype): boolean;

(* <top>: *)
(*             <tertiary> ok*)
(*             |<tertiary> and <top> ok*)
(*             |<tertiary> or <top> ok*)
(*<tertiary>:*)
(*             | both <tertiary> and <top> ok*)
(*             | either <tertiary> or <top> ok*)
(*             | neither <top> nor <top> ok*)
(*             | iverb *)
(*             | tverb <subj>*)
(*             | tverb itself*)
(*             | is <BP>*)
(*             | is not <BP>*)
(*             | does <DP>*)
(*             | does not <DP>*)


		function top: boolean;
		forward;

		function tertiary: boolean;
		begin
			tertiary := false;

			case tokentype of
				engword: 
					if (svalue(token) = 'IS') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if (tokentype = engword) & (svalue(token) = 'NOT') then
								getpredlisttoken(thislist, token, tokentype);
							tertiary := BPParse(thislist, token, tokentype);
						end
					else if (svalue(token) = 'DOES') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if (tokentype = engword) & (svalue(token) = 'NOT') then
								getpredlisttoken(thislist, token, tokentype);
							tertiary := DPParse(thislist, token, tokentype);
						end
					else if (svalue(token) = 'BOTH') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'AND') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end
					else if (svalue(token) = 'EITHER') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'OR') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end
					else if (svalue(token) = 'NEITHER') then
						begin
							getpredlisttoken(thislist, token, tokentype);
							if tertiary then
								begin
									if (tokentype = engword) & (svalue(token) = 'NOR') then
										begin
											getpredlisttoken(thislist, token, tokentype);
											tertiary := top;
										end
								end;
						end;


				iverb: 
					begin
						tertiary := true;
						getpredlisttoken(thislist, token, tokentype);   (*alters thislist locally by advancing*)
					end;

				tverb: 
					begin
						getpredlisttoken(thislist, token, tokentype);   (*alters thislist locally by advancing*)
						if (tokentype = engword) & ((svalue(token) = 'ITSELF') | (svalue(token) = 'HIMSELF') | (svalue(token) = 'HERSELF')) then
							begin
								tertiary := true;
								getpredlisttoken(thislist, token, tokentype);
							end
						else
							tertiary := SubjParse(thislist, token, tokentype);
					end;


				otherwise
			end;
		end;



		function top: boolean;
			var
				altToken, altList: integer;
				altTokenType: listtokentype;
				dummy: boolean;
		begin
			top := false;

			if tertiary then
				begin
					top := true;

(*what we attempt to do now is to consume as much input as possible-- if we*)
(*encounter eg  'goes and' we dont know whether the 'and' connects up a further*)
(*part of the verb phrase or another sentence entirely. So we try to make it part*)
(*of the verb phrase and if we fail we backtrack*)

					if (tokentype = engword) & ((svalue(token) = 'AND') | (svalue(token) = 'OR')) then
						begin
							altToken := token;
							altList := thislist;
							altTokenType := tokentype;
							getpredlisttoken(altList, altToken, altTokentype);
							if VPParse(altList, altToken, altTokentype) then (*if we can advance we do*)
								begin
									getpredlisttoken(thislist, token, tokentype);
									dummy := top;
								end;

						end

				end;
		end;

	begin
		VPParse := top;
	end;



(**************************************)




	function find_matching_string (thislist: integer; var index: integer; findstring, avoidstring: string): boolean;
	forward;


	function proposition_parse (thislist: integer): boolean;

(* <top>: <tertiary> | <tertiary> connective <top> *)
(*| both <top> and <top>*)
(*| either <top> or <top>*)
(*| neither <top> nor <top>*)
(*| if  <top> then <top>*)
		var
			token, righttoken: integer;
			tokentype, righttokentype: listtokentype;

		function top (var thislist: integer): boolean;
		forward;

		function tertiary (var thislist: integer): boolean;

(*<tertiary>:  It is not the case that <proposition>| <proposition>*)

		begin
			getlisttoken(thislist, token, tokentype);   (*alters thislist locally by advancing*)
			if (tokentype = english_neg) then
				begin
					tertiary := tertiary(thislist);
				end
			else
				tertiary := (tokentype = proposition);
		end;


		function top (var thislist: integer): boolean;
			var
				firstproposition, secondproposition, dividing_index: integer;
		begin
			top := false;

			if tertiary(thislist) then
				begin
					top := true;

					getlisttoken(thislist, token, tokentype);

					if tokentype = english_connective then
						begin
							top := top(thislist);
(*remember thislist is now the rhs*)
						end;
				end
			else if (tokentype = both) then
				begin
					if find_matching_string(thislist, dividing_index, 'AND', 'BOTH') then
						begin

							secondproposition := thislist;
							firstproposition := firstn(secondproposition, (dividing_index));

							secondproposition := cdr(secondproposition);

							top := top(firstproposition) & top(secondproposition);

						end;
				end
			else if (tokentype = either) then
				begin
					if find_matching_string(thislist, dividing_index, 'OR', 'EITHER') then
						begin

							secondproposition := thislist;
							firstproposition := firstn(secondproposition, (dividing_index));

							secondproposition := cdr(secondproposition);

							top := top(firstproposition) & top(secondproposition);

						end;
				end
			else if (tokentype = neither) then
				begin
					if find_matching_string(thislist, dividing_index, 'NOR', 'NEITHER') then
						begin
							secondproposition := thislist;
							firstproposition := firstn(secondproposition, (dividing_index));

							secondproposition := cdr(secondproposition);

							top := top(firstproposition) & top(secondproposition);

						end;
				end

			else if (tokentype = i_f) then
				begin
					if top(thislist) then
						begin
(*getlisttoken(thislist, token, tokentype);*)
							if tokentype = the_n then
								top := top(thislist);
						end;
				end;
		end;




	begin
		proposition_parse := false;

		if top(thislist) then
			begin
(*getlisttoken(thislist, token, tokentype); *)

				if tokentype = endlist then
					proposition_parse := true
			end;
	end;

	function find_matching_string (thislist: integer; var index: integer; findstring, avoidstring: string): boolean;
{we are looking for the first matching findstring of a matching pair and, if we meet an avoidstring}
{we must increase count; for example both... and, when we meet a both we look for an and}
{but if we meet a second both then we are looking for the second and}
		var
			n, search: integer;
			found: boolean;
			key: string;

	begin
		n := 0;
		index := 0;
		found := false;
		search := thislist;
		while not found & (search <> gNil) do
			begin
				key := svalue(first(search));
				if (key = findstring) then
					begin
						if n = 0 then
							found := true
						else
							n := n - 1;
					end;

				if (key = avoidstring) then
					n := n + 1;

				if not found then
					begin
						search := cdr(search);
						index := index + 1;

					end;
			end;
		find_matching_string := found;
	end;


(*********** parsing routines ***************)


(************)


(************************************)







	procedure SymbolizeOneStep (predicate: boolean; english_word_list: integer; var success: boolean);

{If the result is symbolic we need to write it back without blanks in between}

		var
			symbolization: integer;
			english: boolean;

		procedure format (symbolization: integer);
			var
				search: integer;
				message: string;
		begin
			search := symbolization;

			message := '{Ambiguous!';
			gOutputStream.WriteStringWithoutLengthByte(@message);
			gOutPutStream.WriteCharacter(gReturn);

			while search <> gNil do
				begin
					putexp_omitting_outerbrackets(car(search));
					search := cdr(search);
					gOutPutStream.WriteCharacter(gReturn);
				end;
			gOutPutStream.WriteCharacter('}');
		end;


	begin
		gOutputStr := kNull;
		english := true;




		if predicate then
			symbolization := try_all_the_rules(english_word_list, english)  (*predicate analysis*)
		else
			symbolization := try_all_the_propositional_rules(english_word_list, english); (*prop analysis*)


		if symbolization = gNil then
			success := false
		else
			begin
				success := true;

				gOutputStream.SetForWritingTo;

				if listlength(symbolization) = 1 then
					begin
						if english then
							putexp_omitting_outerbrackets(first(symbolization))(*to gOutputStream*)
						else
							implodeList(first(symbolization))(*to gOutputStream*)
					end
				else
					format(symbolization);



				symbolization := 0;
				english_word_list := 0;
			end;

		if CollectionWise then
			if collectGarbage then
				; (*mf remove*)

	end;

	function LookupTransVerb (predicate: string): integer;
		var
			pair, value: integer;

	begin
		gEOF := false;
		value := InternalStringtoSExpression(predicate);

		pair := rassoc(value, gTransitive_Verbs);

		if (pair <> gNil) then
			LookupTransVerb := extract_key(pair)
		else
			LookupTransVerb := gNil;
	end;

	function LookupPassiveVerb (predicate: string): integer;
		var
			pair, value: integer;

	begin
		gEOF := false;
		value := InternalStringtoSExpression(predicate);

		pair := rassoc(value, gPassiveVerbs);

		if (pair <> gNil) then
			LookupPassiveVerb := extract_key(pair)
		else
			LookupPassiveVerb := gNil;
	end;

	function LookupAdj (predicate: string): integer;
		var
			pair, value: integer;

	begin
		gEOF := false;
		value := InternalStringtoSExpression(predicate);

		pair := rassoc(value, gAdjectives);

		if (pair <> gNil) then
			LookupAdj := extract_key(pair)
		else
			LookupAdj := gNil;
	end;

	function LookupBinaryAdj (predicate: string): integer;
		var
			pair, value: integer;

	begin
		gEOF := false;
		value := InternalStringtoSExpression(predicate);

		pair := rassoc(value, gBinaryAdjs);

		if (pair <> gNil) then
			LookupBinaryAdj := extract_key(pair)
		else
			LookupBinaryAdj := gNil;
	end;

	function LookupNoun (predicate: string): integer;
		var
			pair, value: integer;

	begin
		gEOF := false;
		value := InternalStringtoSExpression(predicate);

		pair := rassoc(value, gNouns);

		if (pair <> gNil) then
			LookupNoun := extract_key(pair)
		else
			LookupNoun := gNil;
	end;

	function LookupInTransVerb (predicate: string): integer;
		var
			pair, value: integer;

	begin
		gEOF := false;
		value := InternalStringtoSExpression(predicate);

		pair := rassoc(value, gIntransitive_Verbs);

		if (pair <> gNil) then
			LookupInTransVerb := extract_key(pair)
		else
			LookupInTransVerb := gNil;
	end;

	function LookupProposition (proposition: string): integer;
		var
			pair, value: integer;

	begin
		gEOF := false;
		value := InternalStringtoSExpression(proposition);

		pair := rassoc(value, gPropositions);

		if (pair <> gNil) then
			LookupProposition := extract_key(pair)
		else
			LookupProposition := gNil;
	end;

	function LookupTerm (term: string): integer;
		var
			pair, value: integer;

	begin
		gEOF := false;
		value := InternalStringtoSExpression(term);

		pair := rassoc(value, gNames);

		if (pair <> gNil) then
			LookupTerm := extract_key(pair)
		else
			begin
				pair := rassoc(value, gEngVariables);
				if (pair <> gNil) then
					LookupTerm := extract_key(pair)
				else
					LookupTerm := gNil;
			end;
	end;

	function TranslateBack (thisformula: TFormula; aParser: TParser): boolean;

		function TranslateBackFormula (root: TFormula; var outputStream: THandleStream): boolean;

			var
				message, key: string;
				temp: integer;

			procedure TranslateAtomic;
				var
					term1, term2: integer;
			begin


				case root.Arity of
					0: 
						begin
							key := root.fInfo;
							temp := LookupProposition(key);
							if (temp <> gNil) then
								putexp_omitting_outerbrackets(temp)
							else
								TranslateBackFormula := false;   (*prop not known*)
						end;
					1: 
						begin
							key := root.FirstTerm.fInfo;
							term1 := LookupTerm(key);

							if (term1 = gNil) then
								TranslateBackFormula := false  (*term not known*)
							else
								begin
									key := root.fInfo;
									temp := LookupInTransVerb(key);
									if (temp <> gNil) then
										putexp_omitting_outerbrackets(append(term1, temp))
									else
										begin
											temp := LookupNoun(key);
											if (temp <> gNil) then
												begin
													putexp_omitting_outerbrackets(term1);
													message := 'IS A  ';
													outPutStream.WriteStringWithoutLengthByte(@message);
													putexp_omitting_outerbrackets(temp);
												end
											else
												begin
													temp := LookupAdj(key);
													if (temp <> gNil) then
														begin
															putexp_omitting_outerbrackets(term1);
															message := 'IS ';
															outPutStream.WriteStringWithoutLengthByte(@message);
															putexp_omitting_outerbrackets(temp);
														end
													else
														TranslateBackFormula := false;   (*predicate not known*)
												end;
										end;
								end;
						end;

					2: 
						begin
							key := root.FirstTerm.fInfo;
							term1 := LookupTerm(key);

							if (term1 = gNil) then
								TranslateBackFormula := false  (*term not known*)
							else
								begin
									key := root.SecondTerm.fInfo;
									term2 := LookupTerm(key);

									if (term2 = gNil) then
										TranslateBackFormula := false  (*term not known*)
									else
										begin
											key := root.fInfo;
											temp := LookupTransVerb(key);

											if (temp <> gNil) then
												putexp_omitting_outerbrackets(append(term1, append(temp, term2)))
											else
												begin
													temp := LookupPassiveVerb(key);
													if (temp <> gNil) then
														begin
															putexp_omitting_outerbrackets(term1);
															message := ' IS ';
															outPutStream.WriteStringWithoutLengthByte(@message);
															putexp_omitting_outerbrackets(temp);
															putexp_omitting_outerbrackets(term2);
														end
													else
														begin
															temp := LookupBinaryAdj(key);
															if (temp <> gNil) then
																begin
																	putexp_omitting_outerbrackets(term1);
																	message := ' IS ';
																	outPutStream.WriteStringWithoutLengthByte(@message);
																	putexp_omitting_outerbrackets(temp);
																	putexp_omitting_outerbrackets(term2);
																end
															else
																TranslateBackFormula := false;   (*predicate not known*)
														end;
												end;
										end;
								end;
						end;
					otherwise
				end;


			end;

		begin
			TranslateBackFormula := true;
			message := kNull;

			if root <> nil then
				begin
					case root.fKind of
						predicator: 
							TranslateAtomic;
						functor: 
							aParser.WriteTermToStream(root, outPutStream);
						variable: 
							aParser.WriteTermToStream(root, outPutStream);
						equality: {check}
							begin
								outPutStream.WriteCharacter('(');
								aParser.WriteTermToStream(root.fRlink.fLLink, outPutStream);

								outPutStream.WriteCharacter(root.fInfo[1]);

								aParser.WriteTermToStream(root.fRlink.fRlink.fLLink, outPutStream);
								outPutStream.WriteCharacter(')');
							end;
						unary: 
							begin
								message := 'IT IS NOT THE CASE THAT  ';
								outPutStream.WriteStringWithoutLengthByte(@message);
								aParser.WriteInnerToStream(root.fRlink, outPutStream);
							end;

						binary: 
							if (root.fInfo = chImplic) then
								begin
									message := 'IF  ';
									outPutStream.WriteStringWithoutLengthByte(@message);
									aParser.WriteInnerToStream(root.fLlink, outPutStream);
									message := ' THEN ';
									outPutStream.WriteStringWithoutLengthByte(@message);
									aParser.WriteInnerToStream(root.fRlink, outPutStream);
								end
							else
								begin
									aParser.WriteInnerToStream(root.fLlink, outPutStream);

									case root.fInfo[1] of
										chAnd: 
											message := ' AND ';
										chOr: 
											message := ' OR ';
										chEquiv: 
											message := ' IF, AND ONLY IF, ';

										otherwise
									end;

									outPutStream.WriteStringWithoutLengthByte(@message);

									aParser.WriteInnerToStream(root.fRlink, outPutStream);
								end;

						quantifier: 
							begin
								if (root.fInfo = chUniquant) then
									begin
										message := 'FOR ALL ';
										outPutStream.WriteStringWithoutLengthByte(@message);
										putexp_omitting_outerbrackets(LookUpTerm(root.QuantVar));
										message := ', ';
										outPutStream.WriteStringWithoutLengthByte(@message);
									end
								else
									begin
										message := 'THERE IS AN ';
										outPutStream.WriteStringWithoutLengthByte(@message);
										putexp_omitting_outerbrackets(LookUpTerm(root.QuantVar));
										message := ' SUCH THAT ';
										outPutStream.WriteStringWithoutLengthByte(@message);
									end;

								aParser.WriteInnerToStream(root.Scope, outPutStream);
							end;

						otherwise
					end;
				end;
		end;

	begin
		TranslateBack := false;
		gOutputStream.SetForWritingTo;

(*aParser.WriteFormulaToStream(thisFormula, gOutputStream); *)


		TranslateBack := TranslateBackFormula(thisformula, gOutputStream);
	end;














(**************** predicates ********************)






	function oldgeneral_match (word_list, variable_pair_list: integer): integer;
		var
			result_list, target_length, no_of_variables, test, count, symbolization: integer;
			tail_assoc_list: integer;
	begin
		result_list := gNil;
		target_length := listlength(word_list);
		no_of_variables := listlength(variable_pair_list);

		if endp(word_list) & endp(variable_pair_list) then
			oldgeneral_match := gT
		else if endp(word_list) | endp(variable_pair_list) then
			oldgeneral_match := gNil
		else
			begin
				test := extract_test(first(variable_pair_list));

				for count := 0 to (target_length - no_of_variables) do (*off by one error here*)
					begin
						symbolization := fundispatch(test, butlast(word_list, target_length - (count + 1)));
						if not (symbolization = gNil) then
							begin

								tail_assoc_list := general_match(nthcdr((count + 1), word_list), cdr(variable_pair_list));
								if not (tail_assoc_list = gNil) then
									begin

										if (tail_assoc_list = gT) then
											tail_assoc_list := gNil;

										result_list := append(result_list, add_binding_to_each_member_of_list(extract_var(first(variable_pair_list)), butlast(word_list, (target_length - (count + 1))), symbolization, tail_assoc_list));
									end;
							end;
					end;
				oldgeneral_match := result_list;
			end;
	end;














(********** processing *****************)


(*problem is that the value of a variable is a list and we want this flattened*)

	function oldinstantiate_variables (pattern, a_list: integer): integer;
	begin
		if isatom(pattern) then
			oldinstantiate_variables := pattern
		else if variable_p(pattern) then
			oldinstantiate_variables := extract_value(find_binding(pattern, a_list))
		else
			oldinstantiate_variables := cons(instantiate_variables(car(pattern), a_list), instantiate_variables(cdr(pattern), a_list));
	end;

	function instantiate_variables (pattern, a_list: integer): integer;
	begin
		if endp(pattern) then
			instantiate_variables := gNil
		else

			if isatom(first(pattern)) then
				instantiate_variables := cons(first(pattern), instantiate_variables(cdr(pattern), a_list))
			else if variable_p(first(pattern)) then
				instantiate_variables := append(extract_value(find_binding(first(pattern), a_list)), instantiate_variables(cdr(pattern), a_list))
			else
				instantiate_variables := cons(instantiate_variables(car(pattern), a_list), instantiate_variables(cdr(pattern), a_list));
	end;

	function symbolize_variables (pattern, a_list: integer): integer;
	begin
		if endp(pattern) then
			symbolize_variables := gNil
		else

			if isatom(first(pattern)) then
				symbolize_variables := cons(first(pattern), symbolize_variables(cdr(pattern), a_list))
			else if variable_p(first(pattern)) then
{mf next line used to use append not cons}
				symbolize_variables := cons(extract_symbolization(find_binding(first(pattern), a_list)), symbolize_variables(cdr(pattern), a_list))
			else
				symbolize_variables := cons(symbolize_variables(car(pattern), a_list), symbolize_variables(cdr(pattern), a_list));
	end;



	function try_rule (rule, assertion: integer): integer;
(*returns list of successful consequences*)
		var
			list_of_binding_lists, ruleThen: integer;
		function process_list (list: integer): integer;
		begin
			if endp(list) then
				process_list := gNil
			else
				process_list := cons(instantiate_variables(ruleThen, first(list)), process_list(cdr(list)));
		end;

	begin
		list_of_binding_lists := apply_rule_matching_function(rule, cons(assertion, cons(rule_if(rule), gNil)));
		if (list_of_binding_lists = gNil) then
			try_rule := gNil
		else
			begin
				ruleThen := rule_then(rule);
				try_rule := process_list(list_of_binding_lists);
			end;
	end;

	function try_symbolization (rule, assertion: integer): integer;
(*returns list of successful consequences*)
		var
			list_of_binding_lists, ruleThen: integer;
		function process_list (list: integer): integer;


		begin
			if endp(list) then
				process_list := gNil
			else
				begin
					process_list := cons(symbolize_variables(ruleThen, first(list)), process_list(cdr(list)));
				end;
		end;

	begin
		list_of_binding_lists := apply_rule_matching_function(rule, cons(assertion, cons(rule_if(rule), gNil)));

		if (list_of_binding_lists = gNil) then
			try_symbolization := gNil
		else
			begin
				ruleThen := rule_then(rule);
				try_symbolization := process_list(list_of_binding_lists);
			end;
	end;

	function try_all_the_rules (assertion: integer; var english: boolean): integer;

(*here we first we try to symbolize it , then if that fails try to shuffle it around as english*)
		var
			temp: integer;
			found: boolean;

		function myfunction (rule: integer): integer;
		begin
			myfunction := try_rule(rule, assertion);
		end;

		function mysecondfunction (rule: integer): integer;
			var
				anothertemp: integer;
		begin
			if found then
				mysecondfunction := gNil
			else
				begin
					anothertemp := try_symbolization(rule, assertion);
					if anothertemp <> gNil then                    (*there is only one symbolization*)
						found := true;
					mysecondfunction := try_symbolization(rule, assertion);
				end;
		end;

	begin
		temp := gNil;
		english := false;
		if endp(assertion) then
			temp := gNil
		else
			begin
				begin
					found := false;
					if gPredSymbolizationRules = gNil then
						InitializeResourceAssociationList(gPredSymbolizationRules, kPredSymbolizationRulesRSRCID);  (*kludge*)
					temp := accumulate(mapcar(mysecondfunction, gPredSymbolizationRules));  (*symbolize*)
				end;

				if (temp = gNil) then
					begin
						english := true;
						if gRules = gNil then
							InitializeResourceAssociationList(gRules, kRulesRSRCID);   (*kludge*)

						temp := accumulate(mapcar(myfunction, gRules));  (*english*)
					end;

			end;
		try_all_the_rules := temp;
	end;


	function try_all_the_propositional_rules (assertion: integer; var english: boolean): integer;

(*here we first try to shuffle it around as english, then if that fails we try to symbolize it*)
		var
			temp: integer;

		function myfunction (rule: integer): integer;
		begin
			myfunction := try_rule(rule, assertion);
		end;

		function mysecondfunction (rule: integer): integer;
		begin
			mysecondfunction := try_symbolization(rule, assertion);
		end;

	begin
		if endp(assertion) then
			temp := gNil
		else
			begin

				temp := accumulate(mapcar(myfunction, gPropositionalRules));  (*english*)

				if (temp = gNil) then
					temp := accumulate(mapcar(mysecondfunction, gPropSymbolizationRules));  (*symbolize*)

			end;
		try_all_the_propositional_rules := temp;
	end;


(* an adjnoun is a noun preceded by zero or more ordinary adjectives*)
(*note this is a straight boolean and not a fun that returns a symbolization*)

	function adjnoun_p (word_list: integer): integer;
		var
			binding: integer;
	begin
		if endp(word_list) then
			adjnoun_p := gNil
		else if (adj_p(cons(first(word_list), gNil)) <> gNil) then
			adjnoun_p := adjnoun_p(cdr(word_list))
		else if (noun_p(cons(first(word_list), gNil)) <> gNil) & (listlength(word_list) = 1) then
			adjnoun_p := gT
		else
			adjnoun_p := gNil;
	end;




(*this tries an individual rule and makes a list of successful consequents *)

	function adj_p (word_list: integer): integer;
		var
			binding: integer;
	begin
		binding := assoc(word_list, gAdjectives);
		if binding = gNil then
			adj_p := gNil
		else
			adj_p := extract_value(binding);
	end;

	function atomic_proposition_p (english_word_list: integer): integer;
		var
			binding: integer;
	begin
		binding := assoc(english_word_list, gPropositions);
		if binding = gNil then
			atomic_proposition_p := gNil
		else
			atomic_proposition_p := extract_value(binding);
	end;

	function badj_p (word_list: integer): integer;
		var
			binding: integer;
	begin
		binding := assoc(word_list, gBinaryAdjs);
		if binding = gNil then
			badj_p := gNil
		else
			badj_p := extract_value(binding);
	end;


	function bp_p (word_list: integer): integer;
		var
			token: integer;
			tokentype: listtokentype;
	begin
		bp_p := gNil;
		getpredlisttoken(word_list, token, tokentype);
		if BPParse(word_list, token, tokentype) then
			if tokentype = endlist then
				bp_p := gT;
	end;


	function dp_p (word_list: integer): integer;
		var
			token: integer;
			tokentype: listtokentype;
	begin
		dp_p := gNil;
		getpredlisttoken(word_list, token, tokentype);
		if DPParse(word_list, token, tokentype) then
			if tokentype = endlist then
				dp_p := gT;
	end;

	function noun_p (word_list: integer): integer;
		var
			binding: integer;
	begin
		binding := assoc(word_list, gNouns);
		if binding = gNil then
			noun_p := gNil
		else
			noun_p := extract_value(binding);
	end;



	function np_p (word_list: integer): integer;
		var
			token: integer;
			tokentype: listtokentype;
	begin
		np_p := gNil;
		getpredlisttoken(word_list, token, tokentype);
		if NPParse(word_list, token, tokentype) then
			if tokentype = endlist then
				np_p := gT;
	end;

	function passv_p (word_list: integer): integer;
		var
			binding: integer;
	begin
		binding := assoc(word_list, gPassiveVerbs);
		if binding = gNil then
			passv_p := gNil
		else
			passv_p := extract_value(binding);
	end;

	function proposition_p (english_word_list: integer): integer;  (*rewrite more efficiently*)
		var
			binding: integer;
	begin
		binding := assoc(english_word_list, gPropositions);
		if binding = gNil then
			begin
				if proposition_parse(english_word_list) then
					proposition_p := gT
				else
					proposition_p := gNil;
			end
		else
			proposition_p := extract_value(binding);
	end;

	function rv_p (word_list: integer): integer;
		var
			token: integer;
			tokentype: listtokentype;
	begin
		rv_p := gNil;
		getpredlisttoken(word_list, token, tokentype);
		if RVParse(word_list, token, tokentype) then
			if tokentype = endlist then
				rv_p := gT;
	end;

	function sentence_p (word_list: integer): integer;

		var
			token: integer;
			tokentype: listtokentype;
	begin
		sentence_p := gNil;
		getpredlisttoken(word_list, token, tokentype);
		if SentenceParse(word_list, token, tokentype) then
			if tokentype = endlist then
				sentence_p := gT;
	end;

	function subject_p (word_list: integer): integer;
		var
			token: integer;
			tokentype: listtokentype;
	begin
		subject_p := gNil;
		getpredlisttoken(word_list, token, tokentype);
		if SubjParse(word_list, token, tokentype) then
			if tokentype = endlist then
				subject_p := gT;
	end;

	function term_p (word_list: integer): integer;
		var
			binding: integer;
	begin
		term_p := gNil;
		if (listlength(word_list) = 1) then   (*restrict terms to length 1*)
			begin
				binding := assoc(word_list, gNames);
				if binding <> gNil then
					term_p := extract_value(binding)
				else
					begin
						binding := assoc(word_list, gEngVariables);
						if binding <> gNil then
							term_p := extract_value(binding)
					end;
			end;
	end;

	function tvp_p (word_list: integer): integer;
		var
			binding: integer;
	begin
		binding := assoc(word_list, gTransitive_Verbs);
		if binding = gNil then
			tvp_p := gNil
		else
			tvp_p := extract_value(binding);
	end;
	function vip_p (word_list: integer): integer;
		var
			binding: integer;
	begin
		binding := assoc(word_list, gIntransitive_Verbs);
		if binding = gNil then
			vip_p := gNil
		else
			vip_p := extract_value(binding);
	end;

	function vp_p (word_list: integer): integer;
		var
			token: integer;
			tokentype: listtokentype;
	begin
		vp_p := gNil;
		getpredlisttoken(word_list, token, tokentype);
		if VPParse(word_list, token, tokentype) then
			if tokentype = endlist then
				vp_p := gT;
	end;









	function add_binding_to_each_member_of_list (variable_expr, datum, symbolization, list_of_assoc_lists: integer): integer;
		var
			list: integer;
		function myfunction (assoc_list: integer): integer;
		begin
			myfunction := add_special_binding(variable_expr, datum, symbolization, assoc_list);
		end;
	begin
		if endp(list_of_assoc_lists) then
			add_binding_to_each_member_of_list := cons(add_special_binding(variable_expr, datum, symbolization, gNIL), gNil)
		else
			add_binding_to_each_member_of_list := mapcar(myfunction, list_of_assoc_lists);

	end;

	procedure WriteUnaryAssocList (list: integer);
		var
			dummy: integer;

		function WriteEach (item: integer): integer;   (*works by side-effect*)
		begin
			putexp_omitting_outerbrackets(car(cdr(item)));
			gOutputStream.SetPosition(gOutputStream.GetPosition - 1);  (*removes the blank*)
			gOutputStream.WriteCharacter('x');

			gOutputStream.WriteCharacter(chBlank);
			gOutputStream.WriteCharacter(chEquals);
			gOutputStream.WriteCharacter(chBlank);

			putexp_omitting_outerbrackets(car(item));
			gOutputStream.WriteCharacter(gReturn);
			WriteEach := 0;
		end;

	begin
(*gOutputStream.SetForWritingTo;*)
		dummy := mapcar(writeEach, list);
	end;


	procedure WriteBinaryAssocList (list: integer);
		var
			dummy: integer;

		function WriteEach (item: integer): integer;   (*works by side-effect*)
		begin
			putexp_omitting_outerbrackets(car(cdr(item)));
			gOutputStream.SetPosition(gOutputStream.GetPosition - 1);  (*removes the blank*)
			gOutputStream.WriteCharacter('x');
			gOutputStream.WriteCharacter('y');

			gOutputStream.WriteCharacter(chBlank);
			gOutputStream.WriteCharacter(chEquals);
			gOutputStream.WriteCharacter(chBlank);

			putexp_omitting_outerbrackets(car(item));
			gOutputStream.WriteCharacter(gReturn);
			WriteEach := 0;
		end;

	begin
(*gOutputStream.SetForWritingTo;*)
		dummy := mapcar(writeEach, list);
	end;

	function oldbpParse (thislist: integer): boolean;
	forward; (*remove this later*)

	function oldbpParse (thislist: integer): boolean;

(* <top>: *)
(*| both <top> and <top>*)
(*| either <top> or <top>*)
(*| neither <top> nor <top>*)
(*|<tertiary>*)
(* | <tertiary> and <top> *)
(* | <tertiary> or <top> *)


		var
			token, righttoken: integer;
			tokentype, righttokentype: listtokentype;

		function top (var thislist: integer): boolean;
		forward;

		function tertiary (var thislist: integer): boolean;

(*<tertiary>:  a <np>*)
(*|<vp> <subj>*)
(*| <vp> itself him her*)
(*| <adj2> <subj>*)
(*| <adj2> itself*)
(*| <adj>*)


		begin
			getpredlisttoken(thislist, token, tokentype);   (*alters thislist locally by advancing*)

			if (tokentype = english_connective) then
				if (svalue(token) = 'A') then
					begin
						tertiary := (np_p(thislist) <> gNil);
					end
				else
					tertiary := false;   (*change this*)
		end;


		function top (var thislist: integer): boolean;
(* <top>: *)
(*| both <top> and <top>*)
(*| either <top> or <top>*)
(*| neither <top> nor <top>*)
(*|<tertiary>*)
			var
				firstbp, secondbp, dividing_index: integer;
		begin
			top := false;

			if tertiary(thislist) then    (*primes gettoken*)
				begin
					top := true;

					getpredlisttoken(thislist, token, tokentype);

					if tokentype = english_connective then
						if (svalue(token) = 'AND') or (svalue(token) = 'OR') then
							begin
								top := top(thislist);
(*remember thislist is now the rhs*)
							end;
				end
			else

				if (tokentype = both) then
					begin
						if find_matching_string(thislist, dividing_index, 'AND', 'BOTH') then
							begin
								secondbp := thislist;
								firstbp := firstn(secondbp, (dividing_index));
								secondbp := cdr(secondbp);
								top := top(firstbp) & top(secondbp);
							end;
					end
				else if (tokentype = either) then
					begin
						if find_matching_string(thislist, dividing_index, 'OR', 'EITHER') then
							begin
								secondbp := thislist;
								firstbp := firstn(secondbp, (dividing_index));
								secondbp := cdr(secondbp);
								top := top(firstbp) & top(secondbp);
							end;
					end
				else if (tokentype = neither) then
					begin
						if find_matching_string(thislist, dividing_index, 'NOR', 'NEITHER') then
							begin
								secondbp := thislist;
								firstbp := firstn(secondbp, (dividing_index));
								secondbp := cdr(secondbp);
								top := top(firstbp) & top(secondbp);
							end;
					end

		end;




	begin
		oldbpParse := false;

		if top(thislist) then
			begin
				if tokentype = endlist then
					oldbpParse := true
			end;
	end;





end.