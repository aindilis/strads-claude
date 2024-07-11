:- use_module(library(random)).

:- dynamic believe/2.
:- dynamic annal_event/2.

:- discontiguous log/3.

%% :- consult('/var/lib/myfrdcsa/codebases/minor/free-life-planner/lib/util/util.pl').

trace_no_wait :-
	leash(-all),
	trace.

% Log levels
:- dynamic log_level/1.
log_level(debug). % Default log level

% Logging predicate with levels and indentation
log(Level, Message, Indent) :-
	log_level(CurrentLevel),
	log_level_value(CurrentLevel, CurrentValue),
	log_level_value(Level, LevelValue),
	(   LevelValue =< CurrentValue -> 
	    (	
		indent_string(Indent, IndentString),
		format('~w~w: ~w~n', [IndentString, Level, Message])
	    ) ;
	    true).

% Log level values
log_level_value(debug, 3).
log_level_value(info, 2).
log_level_value(warning, 1).
log_level_value(error, 0).

% Helper predicate to create indentation string
indent_string(0, '') :- !.
indent_string(1, '  ') :- !.
indent_string(2, '    ') :- !.
indent_string(3, '      ') :- !.
indent_string(4, '        ') :- !.
indent_string(5, '          ') :- !.
indent_string(_, '            '). % Default for any larger indentation

% Main entry point
start(Flags) :-
	foreach(member(Flag, Flags), assert(Flag)),
	log(info, 'Initializing simulation...', 0),
	initialize_world_state,
	initial_events(InitialEvents),
	assert(events(InitialEvents)),
	log(info, 'Initial events:', 0),
	log(info, InitialEvents, 2),
	main_loop(0).

% Initialize world state
initialize_world_state :-
	retractall(world_state(_, _)),
	assert(world_state(time, 0)),
	assert(world_state(location, home)),
	assert(world_state(day_of_week, monday)),
	assert(world_state(season, spring)),
	assert(world_state(economic_condition, stable)),
	log(info, 'World state initialized:', 0),
	findall(State-Value, world_state(State, Value), WorldState),
	log(info, WorldState, 2).

% Main simulation loop
main_loop(Depth) :-
	MaxDepth = 30, % Adjust this value to control the number of cycles
	Depth < MaxDepth,
	(   events([Event|RestEvents]) ->
	    (	
		NextDepth is Depth + 1,
		log(info, 'Starting new cycle', Depth),
		log(info, 'Current event:', Depth),
		log(info, Event, Depth + 2),
		assert(annal_event(Event, Depth)),
		process_events([Event], Depth + 2),
		retractall(events(_)),
		assert(events(RestEvents)),

		increment_time,
		summarize_cycle(Depth),
		main_loop(NextDepth)
	    )
	;   generate_random_event(NewEvent),
	    log(info, 'Generated random event:', Depth),
	    log(info, NewEvent, Depth + 2),
	    assert(events([NewEvent])),
	    main_loop(Depth)
	).

main_loop(_).

generate_random_event(do(Actor, Action)) :-
	findall(A, atomic_personality(A, _), Actors),
	random_member(Actor, Actors),
	findall(Act, clause(atomic_reaction(do(Actor, Act), _), _), Actions),
	random_member(Action, Actions).

% Process events
process_events([], _).
process_events([Event|RestEvents], Depth) :-
	log(info, 'Processing event:', Depth),
	log(info, Event, Depth + 2),
	update_world_state([Event], Depth + 2),
	identify_affected_actors([Event], Actors),
	determine_reactions([Event], Actors, AllReactions, Depth + 2),
	update_beliefs(AllReactions, Depth + 2),
	collect_new_events(AllReactions, NewEvents),
	log(info, 'New events generated:', Depth + 2),
	log(info, NewEvents, Depth + 4),
	append(NewEvents, RestEvents, UpdatedEvents),
	retractall(events(_)),
	assert(events(UpdatedEvents)),
	log(info, 'Updated event queue:', Depth + 2),
	log(info, UpdatedEvents, Depth + 4).

% Increment time
increment_time :-
	retract(world_state(time, OldTime)),
	NewTime is OldTime + 1,
	assert(world_state(time, NewTime)),
	log(info, 'Time incremented:', 2),
	log(info, time:OldTime->NewTime, 4).

% Update world state
update_world_state([Event], Depth) :-
	log(info, 'Updating world state...', Depth),
	update_single_event(Event, Depth + 2),
	log_world_state(Depth + 2).

update_single_event(do(Actor, Action), Depth) :-
	(   atomic_outcome(Action, Outcome) ->
	    assert_world_state_change(Outcome, Depth),
	    log(info, 'Event outcome:', Depth),
	    NewDepth is Depth + 2,
	    log(info, Actor-Action-Outcome, NewDepth)
	;   log(warning, 'No specific outcome for:', Depth),
	    NewDepth is Depth + 2,
	    log(warning, Actor-Action, NewDepth)
	).

assert_world_state_change(change(Attribute, NewValue), Depth) :-
	(   retract(world_state(Attribute, OldValue)) ->
	    assert(world_state(Attribute, NewValue)),
	    log(info, 'World state change:', Depth),
	    log(info, Attribute:OldValue->NewValue, Depth + 2)
	;   assert(world_state(Attribute, NewValue)),
	    log(info, 'New world state attribute:', Depth),
	    log(info, Attribute:NewValue, Depth + 2)
	).

% Identify actors affected by events
identify_affected_actors(Events, Actors) :-
	findall(Actor, 
		(   member(do(Actor, _), Events) ; 
		    (	member(Event, Events), affected_by_event(Actor, Event))
		), 
		ActorSet),
	list_to_set(ActorSet, Actors).

affected_by_event(Actor, Events) :-
	member(do(_, Action), Events),
	atomic_affect(Action, Actor).

% Determine reactions for actors
determine_reactions(Events, Actors, AllReactions, Depth) :-
	findall([Actor, Reactions],
		(   member(Actor, Actors),
		    actor_perceive_events(Actor, Events, Depth + 2),
		    findall(Reaction, (consider_reaction(Actor, Reaction), Reaction \= do(_, react_to(_))), PossibleReactions),
		    select_reactions(PossibleReactions, Reactions)
		),
		AllReactions).

actor_perceive_events(Actor, Events, Depth) :-
	foreach(member(Event, Events),
		(   assert(perceive(Actor, Event)),
		    log(debug, 'Actor perception:', Depth),
		    log(debug, Actor-Event, Depth + 2))).

% Consider reaction with depth limit and cycle detection
consider_reaction(Actor, Reaction) :-
	perceive(Actor, Event),
	(   atomic_reaction(Event, Reaction)
	;   Event = do(_, Action),
	    atomic_reaction(do(Actor, Action), Reaction)
	).

consider_reaction_depth(_, Event, Event, 3, _) :- !. % Max depth of 3
consider_reaction_depth(Actor, Event, Reaction, Depth, SeenEvents) :-
	Depth < 3,
	\+ member(Event, SeenEvents), % Avoid cycles
	atomic_reaction(Event, PossibleReaction),
	NextDepth is Depth + 1,
	consider_reaction_depth(Actor, PossibleReaction, Reaction, NextDepth, [Event|SeenEvents]).
consider_reaction_depth(Actor, Event, do(Actor, react_to(Event)), _, _).

% Select reactions
select_reactions(PossibleReactions, Reactions) :-
	include(reaction_is_compatible, PossibleReactions, CompatibleReactions),
	list_to_set(CompatibleReactions, UniqueReactions), % Remove duplicates
	(   UniqueReactions = [] ->
	    (	PossibleReactions = [] ->
		Reactions = []
	    ;	random_permutation(PossibleReactions, ShuffledReactions),
		take(3, ShuffledReactions, Reactions)
	    )
	;   random_permutation(UniqueReactions, ShuffledReactions),
	    take(3, ShuffledReactions, Reactions)
	).

% Check if a reaction is compatible
reaction_is_compatible(do(Actor, Action)) :-
	atomic_personality(Actor, Traits),
	atomic_reaction_traits(Action, ReactionTraits),
	(   intersection(Traits, ReactionTraits, CommonTraits),
	    CommonTraits \= []
	;   random(0, 100, R),
	    R < 10 % 10% chance of allowing incompatible reactions for variety
	).

% Update beliefs based on reactions
update_beliefs(AllReactions, Depth) :-
	log(info, 'Updating beliefs...', Depth),
	foreach(member([Actor, Reactions], AllReactions),
		update_actor_beliefs(Actor, Reactions, Depth + 2)).

update_actor_beliefs(_, [], _) :- !.
update_actor_beliefs(Actor, [Reaction|Rest], Depth) :-
	(   Reaction = do(Actor, Action),
	    atomic_belief_update(Action, NewBelief) ->
	    retractall(believe(Actor, _)), % Remove old beliefs
	    assert(believe(Actor, NewBelief)),
	    log(info, 'New belief:', Depth),
	    log(info, Actor-NewBelief, Depth + 2)
	;   true
	),
	update_actor_beliefs(Actor, Rest, Depth).

% Collect new events
collect_new_events(AllReactions, NewEvents) :-
	findall(Event,
		(   member([_, Reactions], AllReactions),
		    member(Event, Reactions),
		    Event \= do(_, react_to(_))
		),
		NewEvents).

summarize_cycle(Depth) :-
	log(info, 'Cycle Summary:', Depth),
	log(info, 'World State:', Depth + 2),
	findall(State-Value, world_state(State, Value), WorldState),
	log(info, WorldState, Depth + 4),
	log(info, 'Actor Beliefs:', Depth + 2),
	findall(Actor-Belief, believe(Actor, Belief), Beliefs),
	log(info, Beliefs, Depth + 4).

% Utility predicates
log_world_state(Depth) :-
	findall(State-Value, world_state(State, Value), WorldState),
	log(debug, 'Current world state:', Depth),
	log(debug, WorldState, Depth + 2).

take(N, List, Taken) :-
	length(List, Len),
	MinLen is min(N, Len),
	length(Taken, MinLen),
	append(Taken, _, List).

generate_annals :-
	findall(annal(Event, Depth), annal_event(Event, Depth), Annals),
	sort(2, @=<, Annals, SortedAnnals),
	format('~n~nThe Annals of Our Simulation:~n~n', []),
	generate_narrative(SortedAnnals, 1).

generate_narrative([], _).
generate_narrative([annal(Event, _)|Rest], Day) :-
	event_to_sentence(Event, Sentence),
	format('Day ~w: ~w~n', [Day, Sentence]),
	NextDay is Day + 1,
	generate_narrative(Rest, NextDay).

event_to_sentence(do(Actor, Action), Sentence) :-
	my_atom_string(Actor, ActorStr),
	action_to_phrase(Action, ActionPhrase),
	string_concat(ActorStr, " ", Temp1),
	string_concat(Temp1, ActionPhrase, Sentence).

action_to_phrase(invite_to_party(Host, Guest), Phrase) :-
	my_atom_string(Host, HostStr),
	my_atom_string(Guest, GuestStr),
	format(string(Phrase), "invited ~w to a party", [GuestStr]).
action_to_phrase(accept_invitation(Host), Phrase) :-
	my_atom_string(Host, HostStr),
	format(string(Phrase), "accepted ~w's invitation", [HostStr]).
action_to_phrase(decline_invitation(Host), Phrase) :-
	my_atom_string(Host, HostStr),
	format(string(Phrase), "declined ~w's invitation", [HostStr]).
action_to_phrase(start_new_job(Person, Company), Phrase) :-
	my_atom_string(Company, CompanyStr),
	format(string(Phrase), "started a new job at ~w", [CompanyStr]).
action_to_phrase(cause_rainy_day, "caused a rainy day").
action_to_phrase(react_to_rain, "reacted to the rain").
action_to_phrase(stay_indoors, "decided to stay indoors").
action_to_phrase(go_out_with_umbrella, "went out with an umbrella").
action_to_phrase(study_for_exam(Subject), Phrase) :-
	my_atom_string(Subject, SubjectStr),
	format(string(Phrase), "studied for the ~w exam", [SubjectStr]).
action_to_phrase(take_exam(Subject), Phrase) :-
	my_atom_string(Subject, SubjectStr),
	format(string(Phrase), "took the ~w exam", [SubjectStr]).
action_to_phrase(exercise(_), "exercised").
action_to_phrase(feel_energized, "felt energized").
action_to_phrase(plan_next_workout, "planned their next workout").
action_to_phrase(feel_nervous(Company), Phrase) :-
	my_atom_string(Company, CompanyStr),
	format(string(Phrase), "felt nervous about working at ~w", [CompanyStr]).
action_to_phrase(feel_excited(Company), Phrase) :-
	my_atom_string(Company, CompanyStr),
	format(string(Phrase), "felt excited about working at ~w", [CompanyStr]).
action_to_phrase(Action, Phrase) :-
	my_atom_string(Action, ActionStr),
	format(string(Phrase), "performed the action: ~w", [ActionStr]).

my_atom_string(Action, Phrase) :-
	(   var(Action) -> Phrase = "unknown-thing" ;
	    (	not(atom(Action)) ->
		with_output_to(string(Phrase),write_term(Action,[quoted(true)])) ;
		atom_string(Action,Phrase))).

% World model and knowledge base

initial_events([
		do(jack, invite_to_party(jack, jill)),
		do(emma, start_new_job(emma, tech_company)),
		do(weather, cause_rainy_day),
		do(alex, study_for_exam(alex, math)),
		do(sophia, exercise(sophia))
	       ]).

% Atomic outcomes
atomic_outcome(invite_to_party(_, _), change(location, party)).
atomic_outcome(start_new_job(_, Company), change(employment_status, employed(Company))).
atomic_outcome(cause_rainy_day, change(weather, rainy)).
atomic_outcome(go_to_movie(_, _), change(location, cinema)).
atomic_outcome(have_dinner(_, _), change(hunger_level, satisfied)).
atomic_outcome(study_for_exam(_, Subject), change(knowledge, increased(Subject))).
atomic_outcome(exercise(_), change(fitness_level, improved)).
atomic_outcome(oversleep(_), change(time, late)).

% Atomic affects
atomic_affect(invite_to_party(_, Target), Target).
atomic_affect(start_new_job(Person, _), Person).
atomic_affect(cause_rainy_day, Actor) :-
	atomic_personality(Actor, _),
	Actor \= weather.
atomic_affect(go_to_movie(_, Target), Target).
atomic_affect(have_dinner(_, Target), Target).
atomic_affect(study_for_exam(Student, _), Student).
atomic_affect(exercise(Person), Person).
atomic_affect(oversleep(Person), Person).

% Atomic reactions
atomic_reaction(do(_, invite_to_party(Host, Target)), do(Target, decide_on_invitation(Host))).
atomic_reaction(do(Person, accept_invitation(Host)), do(Host, celebrate(Person))).
atomic_reaction(do(Person, adjust_to_new_job(Company)), do(Person, feel_excited(Company))).
atomic_reaction(do(Person, adjust_to_new_job(Company)), do(Person, feel_nervous(Company))).
atomic_reaction(do(Person, await_results(_)), do(Person, receive_bad_grade)).
atomic_reaction(do(Person, await_results), do(Person, receive_bad_grade)).
atomic_reaction(do(Person, await_results(_)), do(Person, receive_good_grade)).
atomic_reaction(do(Person, await_results), do(Person, receive_good_grade)).
atomic_reaction(do(Person, decide_on_invitation(Host)), do(Person, accept_invitation(Host))).
atomic_reaction(do(Person, decide_on_invitation(Host)), do(Person, decline_invitation(Host))).
atomic_reaction(do(Person, decline_invitation(Host)), do(Host, feel_disappointed)).
atomic_reaction(do(Person, exercise(_)), do(Person, feel_energized)).
atomic_reaction(do(Person, exercise(_)), do(Person, feel_tired)).
atomic_reaction(do(Person, feel_energized), do(Person, plan_next_workout)).
atomic_reaction(do(Person, invite_to_party(Host, Target)), do(Target, decide_on_invitation(Host))).
atomic_reaction(do(Person, offer_support(_)), do(Person, feel_good)).
atomic_reaction(do(Person, react_to_rain), do(Person, complain_about_weather)).
atomic_reaction(do(Person, react_to_rain), do(Person, enjoy_rainy_day)).
atomic_reaction(do(Person, react_to_rain), do(Person, go_out_with_umbrella)).
atomic_reaction(do(Person, react_to_rain), do(Person, stay_indoors)).
atomic_reaction(do(Person, receive_bad_grade), do(Person, plan_to_study_more)).
atomic_reaction(do(Person, receive_good_grade), do(Person, celebrate_success)).
atomic_reaction(do(Person, seek_comfort(Friend)), do(Friend, offer_support(Person))).
atomic_reaction(do(Person, start_new_job(_)), do(Person, feel_overwhelmed)).
atomic_reaction(do(Person, start_new_job(_)), do(Person, make_new_friends)).
atomic_reaction(do(Person, start_new_job(Person, Company)), do(Person, adjust_to_new_job(Company))).
atomic_reaction(do(Person, study_for_exam(Subject)), do(Person, feel_confident(Subject))).
atomic_reaction(do(Person, study_for_exam(Subject)), do(Person, feel_stressed(Subject))).
atomic_reaction(do(Person, study_for_exam(_, Subject)), do(Person, take_exam(Subject))).
atomic_reaction(do(Person, take_exam(_)), do(Person, await_results)).
atomic_reaction(do(Person, take_exam(Subject)), do(Person, await_results(Subject))).
atomic_reaction(do(_, study_for_exam(Person, Subject)), do(Person, take_exam(Subject))).
atomic_reaction(do(weather, cause_rainy_day), do(_Person, react_to_rain)).
atomic_reaction(do(weather, cause_rainy_day), do(Person, react_to_rain)).

atomic_reaction(do(Person, feel_disappointed), do(Person, seek_comfort(Friend))) :- 
	Person \= Friend,
	atomic_personality(Friend, Traits),
	member(friendly, Traits).
atomic_reaction(do(weather, cause_rainy_day), do(Person, react_to_rain)) :-
	atomic_personality(Person, _),
	Person \= weather.


% Personalities
atomic_personality(jill, [friendly, outgoing, adventurous]).
atomic_personality(jack, [charming, impulsive, fun_loving]).
atomic_personality(emma, [ambitious, hardworking, introverted]).
atomic_personality(weather, [unpredictable]).
atomic_personality(everyone, [diverse]).
atomic_personality(_, [adaptable]). % Default personality for unknown actors

% Reaction traits
atomic_reaction_traits(decide_on_invitation(_), [social, decisive]).
atomic_reaction_traits(accept_invitation(_), [friendly, outgoing, fun_loving]).
atomic_reaction_traits(decline_invitation(_), [introverted, busy]).
atomic_reaction_traits(celebrate(_), [friendly, outgoing]).
atomic_reaction_traits(feel_disappointed, [emotional]).
atomic_reaction_traits(adjust_to_new_job(_), [adaptable, flexible]).
atomic_reaction_traits(feel_nervous(_), [anxious, introverted]).
atomic_reaction_traits(feel_excited(_), [enthusiastic, outgoing]).
atomic_reaction_traits(react_to_rain, [adaptable]).
atomic_reaction_traits(stay_indoors, [cautious, introverted]).
atomic_reaction_traits(go_out_with_umbrella, [prepared, adaptable]).
atomic_reaction_traits(react_to(_), [adaptable]).

% Belief updates
atomic_belief_update(accept_invitation(_), happy).
atomic_belief_update(adjust_to_new_job(_), focused).
atomic_belief_update(await_results(_), nervous).
atomic_belief_update(celebrate(_), excited).
atomic_belief_update(celebrate_success, happy).
atomic_belief_update(decline_invitation(_), regretful).
atomic_belief_update(exercise(_), energized).
atomic_belief_update(feel_disappointed, sad).
atomic_belief_update(feel_energized, motivated).
atomic_belief_update(feel_excited(_), enthusiastic).
atomic_belief_update(feel_nervous(_), anxious).
atomic_belief_update(feel_nervous(_), uncertain).
atomic_belief_update(go_out_with_umbrella, prepared).
atomic_belief_update(invite_to_party(_, _), social).
atomic_belief_update(plan_next_workout, committed).
atomic_belief_update(plan_to_study_more, determined).
atomic_belief_update(react_to_rain, adaptable).
atomic_belief_update(receive_bad_grade, disappointed).
atomic_belief_update(receive_good_grade, proud).
atomic_belief_update(start_new_job(_, _), excited).
atomic_belief_update(stay_indoors, cozy).
atomic_belief_update(study_for_exam(_, _), prepared).
atomic_belief_update(take_exam(_), anxious).


% Run the simulation
do_start :-
	start([flag(debug, on)]),
	log(info, 'Simulation ended.', 0),
	generate_annals,
	halt.

% Query to start the simulation
:- do_start.