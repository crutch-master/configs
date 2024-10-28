root:
ifeq (,$(wildcard ./makefiles))
	@mkdir makefiles
endif

clean:
	@rm -rf makefiles

pottery:  root
ifeq (,$(wildcard ./makefiles/pottery))
	@echo pottery
	@touch ./makefiles/pottery
endif

irrigation: pottery root
ifeq (,$(wildcard ./makefiles/irrigation))
	@echo irrigation
	@touch ./makefiles/irrigation
endif

writing: pottery root
ifeq (,$(wildcard ./makefiles/writing))
	@echo writing
	@touch ./makefiles/writing
endif

animal_husbandry:  root
ifeq (,$(wildcard ./makefiles/animal_husbandry))
	@echo animal_husbandry
	@touch ./makefiles/animal_husbandry
endif

archery: animal_husbandry root
ifeq (,$(wildcard ./makefiles/archery))
	@echo archery
	@touch ./makefiles/archery
endif

mining:  root
ifeq (,$(wildcard ./makefiles/mining))
	@echo mining
	@touch ./makefiles/mining
endif

masonry: mining root
ifeq (,$(wildcard ./makefiles/masonry))
	@echo masonry
	@touch ./makefiles/masonry
endif

bronze_working: mining root
ifeq (,$(wildcard ./makefiles/bronze_working))
	@echo bronze_working
	@touch ./makefiles/bronze_working
endif

the_wheel: mining root
ifeq (,$(wildcard ./makefiles/the_wheel))
	@echo the_wheel
	@touch ./makefiles/the_wheel
endif

apprenticeship: mining currency horseback_riding root
ifeq (,$(wildcard ./makefiles/apprenticeship))
	@echo apprenticeship
	@touch ./makefiles/apprenticeship
endif

sailing:  root
ifeq (,$(wildcard ./makefiles/sailing))
	@echo sailing
	@touch ./makefiles/sailing
endif

celestial_navigation: sailing astrology root
ifeq (,$(wildcard ./makefiles/celestial_navigation))
	@echo celestial_navigation
	@touch ./makefiles/celestial_navigation
endif

shipbuilding: sailing root
ifeq (,$(wildcard ./makefiles/shipbuilding))
	@echo shipbuilding
	@touch ./makefiles/shipbuilding
endif

astrology:  root
ifeq (,$(wildcard ./makefiles/astrology))
	@echo astrology
	@touch ./makefiles/astrology
endif

drama_poetry: astrology irrigation masonry early_empire mysticism root
ifeq (,$(wildcard ./makefiles/drama_poetry))
	@echo drama_poetry
	@touch ./makefiles/drama_poetry
endif

theology: astrology mysticism drama_poetry root
ifeq (,$(wildcard ./makefiles/theology))
	@echo theology
	@touch ./makefiles/theology
endif

horseback_riding: archery root
ifeq (,$(wildcard ./makefiles/horseback_riding))
	@echo horseback_riding
	@touch ./makefiles/horseback_riding
endif

machinery: archery iron_working engineering root
ifeq (,$(wildcard ./makefiles/machinery))
	@echo machinery
	@touch ./makefiles/machinery
endif

currency: writing foreign_trade root
ifeq (,$(wildcard ./makefiles/currency))
	@echo currency
	@touch ./makefiles/currency
endif

state_workforce: writing bronze_working craftsmanship root
ifeq (,$(wildcard ./makefiles/state_workforce))
	@echo state_workforce
	@touch ./makefiles/state_workforce
endif

recorded_history: writing political_philosophy drama_poetry root
ifeq (,$(wildcard ./makefiles/recorded_history))
	@echo recorded_history
	@touch ./makefiles/recorded_history
endif

construction: masonry the_wheel horseback_riding root
ifeq (,$(wildcard ./makefiles/construction))
	@echo construction
	@touch ./makefiles/construction
endif

engineering: masonry the_wheel root
ifeq (,$(wildcard ./makefiles/engineering))
	@echo engineering
	@touch ./makefiles/engineering
endif

iron_working: bronze_working root
ifeq (,$(wildcard ./makefiles/iron_working))
	@echo iron_working
	@touch ./makefiles/iron_working
endif

mathematics: bronze_working celestial_navigation currency drama_poetry root
ifeq (,$(wildcard ./makefiles/mathematics))
	@echo mathematics
	@touch ./makefiles/mathematics
endif

military_training: bronze_working military_tradition games_recreation root
ifeq (,$(wildcard ./makefiles/military_training))
	@echo military_training
	@touch ./makefiles/military_training
endif

cartography: celestial_navigation shipbuilding root
ifeq (,$(wildcard ./makefiles/cartography))
	@echo cartography
	@touch ./makefiles/cartography
endif

medieval_faires: currency feudalism root
ifeq (,$(wildcard ./makefiles/medieval_faires))
	@echo medieval_faires
	@touch ./makefiles/medieval_faires
endif

guilds: currency feudalism civil_service root
ifeq (,$(wildcard ./makefiles/guilds))
	@echo guilds
	@touch ./makefiles/guilds
endif

mercantilism: currency humanism root
ifeq (,$(wildcard ./makefiles/mercantilism))
	@echo mercantilism
	@touch ./makefiles/mercantilism
endif

stirrups: horseback_riding feudalism root
ifeq (,$(wildcard ./makefiles/stirrups))
	@echo stirrups
	@touch ./makefiles/stirrups
endif

mass_production: shipbuilding machinery education root
ifeq (,$(wildcard ./makefiles/mass_production))
	@echo mass_production
	@touch ./makefiles/mass_production
endif

naval_tradition: shipbuilding defensive_tactics root
ifeq (,$(wildcard ./makefiles/naval_tradition))
	@echo naval_tradition
	@touch ./makefiles/naval_tradition
endif

military_tactics: mathematics root
ifeq (,$(wildcard ./makefiles/military_tactics))
	@echo military_tactics
	@touch ./makefiles/military_tactics
endif

education: mathematics apprenticeship root
ifeq (,$(wildcard ./makefiles/education))
	@echo education
	@touch ./makefiles/education
endif

military_engineering: construction engineering root
ifeq (,$(wildcard ./makefiles/military_engineering))
	@echo military_engineering
	@touch ./makefiles/military_engineering
endif

castles: construction divine_right exploration root
ifeq (,$(wildcard ./makefiles/castles))
	@echo castles
	@touch ./makefiles/castles
endif

games_recreation: construction state_workforce root
ifeq (,$(wildcard ./makefiles/games_recreation))
	@echo games_recreation
	@touch ./makefiles/games_recreation
endif

gunpowder: apprenticeship stirrups military_engineering root
ifeq (,$(wildcard ./makefiles/gunpowder))
	@echo gunpowder
	@touch ./makefiles/gunpowder
endif

printing: machinery education root
ifeq (,$(wildcard ./makefiles/printing))
	@echo printing
	@touch ./makefiles/printing
endif

metal_casting: machinery gunpowder root
ifeq (,$(wildcard ./makefiles/metal_casting))
	@echo metal_casting
	@touch ./makefiles/metal_casting
endif

banking: education stirrups guilds root
ifeq (,$(wildcard ./makefiles/banking))
	@echo banking
	@touch ./makefiles/banking
endif

astronomy: education root
ifeq (,$(wildcard ./makefiles/astronomy))
	@echo astronomy
	@touch ./makefiles/astronomy
endif

military_science: stirrups printing siege_tactics root
ifeq (,$(wildcard ./makefiles/military_science))
	@echo military_science
	@touch ./makefiles/military_science
endif

siege_tactics: castles metal_casting root
ifeq (,$(wildcard ./makefiles/siege_tactics))
	@echo siege_tactics
	@touch ./makefiles/siege_tactics
endif

square_rigging: cartography gunpowder root
ifeq (,$(wildcard ./makefiles/square_rigging))
	@echo square_rigging
	@touch ./makefiles/square_rigging
endif

exploration: cartography mercenaries medieval_faires root
ifeq (,$(wildcard ./makefiles/exploration))
	@echo exploration
	@touch ./makefiles/exploration
endif

industrialization: mass_production square_rigging root
ifeq (,$(wildcard ./makefiles/industrialization))
	@echo industrialization
	@touch ./makefiles/industrialization
endif

scientific_theory: banking astronomy the_enlightenment root
ifeq (,$(wildcard ./makefiles/scientific_theory))
	@echo scientific_theory
	@touch ./makefiles/scientific_theory
endif

colonialism: astronomy mercantilism root
ifeq (,$(wildcard ./makefiles/colonialism))
	@echo colonialism
	@touch ./makefiles/colonialism
endif

ballistics: metal_casting siege_tactics root
ifeq (,$(wildcard ./makefiles/ballistics))
	@echo ballistics
	@touch ./makefiles/ballistics
endif

economics: metal_casting scientific_theory root
ifeq (,$(wildcard ./makefiles/economics))
	@echo economics
	@touch ./makefiles/economics
endif

scorched_earth: metal_casting nationalism root
ifeq (,$(wildcard ./makefiles/scorched_earth))
	@echo scorched_earth
	@touch ./makefiles/scorched_earth
endif

steam_power: industrialization root
ifeq (,$(wildcard ./makefiles/steam_power))
	@echo steam_power
	@touch ./makefiles/steam_power
endif

flight: industrialization scientific_theory economics root
ifeq (,$(wildcard ./makefiles/flight))
	@echo flight
	@touch ./makefiles/flight
endif

steel: industrialization rifling root
ifeq (,$(wildcard ./makefiles/steel))
	@echo steel
	@touch ./makefiles/steel
endif

class_struggle: industrialization ideology root
ifeq (,$(wildcard ./makefiles/class_struggle))
	@echo class_struggle
	@touch ./makefiles/class_struggle
endif

sanitation: scientific_theory urbanization root
ifeq (,$(wildcard ./makefiles/sanitation))
	@echo sanitation
	@touch ./makefiles/sanitation
endif

rifling: ballistics military_science root
ifeq (,$(wildcard ./makefiles/rifling))
	@echo rifling
	@touch ./makefiles/rifling
endif

totalitarianism: military_science ideology root
ifeq (,$(wildcard ./makefiles/totalitarianism))
	@echo totalitarianism
	@touch ./makefiles/totalitarianism
endif

electricity: steam_power mercantilism root
ifeq (,$(wildcard ./makefiles/electricity))
	@echo electricity
	@touch ./makefiles/electricity
endif

radio: steam_power flight conservation root
ifeq (,$(wildcard ./makefiles/radio))
	@echo radio
	@touch ./makefiles/radio
endif

chemistry: sanitation root
ifeq (,$(wildcard ./makefiles/chemistry))
	@echo chemistry
	@touch ./makefiles/chemistry
endif

suffrage: sanitation ideology root
ifeq (,$(wildcard ./makefiles/suffrage))
	@echo suffrage
	@touch ./makefiles/suffrage
endif

replaceable_parts: economics root
ifeq (,$(wildcard ./makefiles/replaceable_parts))
	@echo replaceable_parts
	@touch ./makefiles/replaceable_parts
endif

capitalism: economics mass_media root
ifeq (,$(wildcard ./makefiles/capitalism))
	@echo capitalism
	@touch ./makefiles/capitalism
endif

combined_arms: flight combustion root
ifeq (,$(wildcard ./makefiles/combined_arms))
	@echo combined_arms
	@touch ./makefiles/combined_arms
endif

synthetic_materials: flight plastics root
ifeq (,$(wildcard ./makefiles/synthetic_materials))
	@echo synthetic_materials
	@touch ./makefiles/synthetic_materials
endif

rapid_deployment: flight cold_war root
ifeq (,$(wildcard ./makefiles/rapid_deployment))
	@echo rapid_deployment
	@touch ./makefiles/rapid_deployment
endif

advanced_ballistics: replaceable_parts steel electricity root
ifeq (,$(wildcard ./makefiles/advanced_ballistics))
	@echo advanced_ballistics
	@touch ./makefiles/advanced_ballistics
endif

combustion: steel natural_history root
ifeq (,$(wildcard ./makefiles/combustion))
	@echo combustion
	@touch ./makefiles/combustion
endif

computers: electricity radio suffrage totalitarianism class_struggle root
ifeq (,$(wildcard ./makefiles/computers))
	@echo computers
	@touch ./makefiles/computers
endif

advanced_flight: radio root
ifeq (,$(wildcard ./makefiles/advanced_flight))
	@echo advanced_flight
	@touch ./makefiles/advanced_flight
endif

rocketry: radio chemistry root
ifeq (,$(wildcard ./makefiles/rocketry))
	@echo rocketry
	@touch ./makefiles/rocketry
endif

nanotechnology: radio composites root
ifeq (,$(wildcard ./makefiles/nanotechnology))
	@echo nanotechnology
	@touch ./makefiles/nanotechnology
endif

mass_media: radio urbanization root
ifeq (,$(wildcard ./makefiles/mass_media))
	@echo mass_media
	@touch ./makefiles/mass_media
endif

nuclear_program: chemistry ideology root
ifeq (,$(wildcard ./makefiles/nuclear_program))
	@echo nuclear_program
	@touch ./makefiles/nuclear_program
endif

plastics: combustion root
ifeq (,$(wildcard ./makefiles/plastics))
	@echo plastics
	@touch ./makefiles/plastics
endif

satellites: advanced_flight rocketry root
ifeq (,$(wildcard ./makefiles/satellites))
	@echo satellites
	@touch ./makefiles/satellites
endif

globalization: advanced_flight rapid_deployment space_race root
ifeq (,$(wildcard ./makefiles/globalization))
	@echo globalization
	@touch ./makefiles/globalization
endif

guidance_systems: rocketry advanced_ballistics root
ifeq (,$(wildcard ./makefiles/guidance_systems))
	@echo guidance_systems
	@touch ./makefiles/guidance_systems
endif

space_race: rocketry cold_war root
ifeq (,$(wildcard ./makefiles/space_race))
	@echo space_race
	@touch ./makefiles/space_race
endif

nuclear_fission: advanced_ballistics combined_arms root
ifeq (,$(wildcard ./makefiles/nuclear_fission))
	@echo nuclear_fission
	@touch ./makefiles/nuclear_fission
endif

telecommunications: computers root
ifeq (,$(wildcard ./makefiles/telecommunications))
	@echo telecommunications
	@touch ./makefiles/telecommunications
endif

robotics: computers globalization root
ifeq (,$(wildcard ./makefiles/robotics))
	@echo robotics
	@touch ./makefiles/robotics
endif

lasers: nuclear_fission root
ifeq (,$(wildcard ./makefiles/lasers))
	@echo lasers
	@touch ./makefiles/lasers
endif

cold_war: nuclear_fission ideology root
ifeq (,$(wildcard ./makefiles/cold_war))
	@echo cold_war
	@touch ./makefiles/cold_war
endif

composites: synthetic_materials root
ifeq (,$(wildcard ./makefiles/composites))
	@echo composites
	@touch ./makefiles/composites
endif

stealth_technology: synthetic_materials root
ifeq (,$(wildcard ./makefiles/stealth_technology))
	@echo stealth_technology
	@touch ./makefiles/stealth_technology
endif

social_media: telecommunications professional_sports space_race root
ifeq (,$(wildcard ./makefiles/social_media))
	@echo social_media
	@touch ./makefiles/social_media
endif

nuclear_fusion: lasers root
ifeq (,$(wildcard ./makefiles/nuclear_fusion))
	@echo nuclear_fusion
	@touch ./makefiles/nuclear_fusion
endif

code_of_laws:  root
ifeq (,$(wildcard ./makefiles/code_of_laws))
	@echo code_of_laws
	@touch ./makefiles/code_of_laws
endif

craftsmanship: code_of_laws root
ifeq (,$(wildcard ./makefiles/craftsmanship))
	@echo craftsmanship
	@touch ./makefiles/craftsmanship
endif

foreign_trade: code_of_laws root
ifeq (,$(wildcard ./makefiles/foreign_trade))
	@echo foreign_trade
	@touch ./makefiles/foreign_trade
endif

military_tradition: craftsmanship root
ifeq (,$(wildcard ./makefiles/military_tradition))
	@echo military_tradition
	@touch ./makefiles/military_tradition
endif

early_empire: foreign_trade root
ifeq (,$(wildcard ./makefiles/early_empire))
	@echo early_empire
	@touch ./makefiles/early_empire
endif

mysticism: foreign_trade root
ifeq (,$(wildcard ./makefiles/mysticism))
	@echo mysticism
	@touch ./makefiles/mysticism
endif

political_philosophy: state_workforce early_empire root
ifeq (,$(wildcard ./makefiles/political_philosophy))
	@echo political_philosophy
	@touch ./makefiles/political_philosophy
endif

defensive_tactics: games_recreation political_philosophy root
ifeq (,$(wildcard ./makefiles/defensive_tactics))
	@echo defensive_tactics
	@touch ./makefiles/defensive_tactics
endif

humanism: drama_poetry medieval_faires root
ifeq (,$(wildcard ./makefiles/humanism))
	@echo humanism
	@touch ./makefiles/humanism
endif

mercenaries: military_training feudalism root
ifeq (,$(wildcard ./makefiles/mercenaries))
	@echo mercenaries
	@touch ./makefiles/mercenaries
endif

feudalism: defensive_tactics root
ifeq (,$(wildcard ./makefiles/feudalism))
	@echo feudalism
	@touch ./makefiles/feudalism
endif

civil_service: defensive_tactics recorded_history root
ifeq (,$(wildcard ./makefiles/civil_service))
	@echo civil_service
	@touch ./makefiles/civil_service
endif

divine_right: theology civil_service root
ifeq (,$(wildcard ./makefiles/divine_right))
	@echo divine_right
	@touch ./makefiles/divine_right
endif

diplomatic_service: guilds root
ifeq (,$(wildcard ./makefiles/diplomatic_service))
	@echo diplomatic_service
	@touch ./makefiles/diplomatic_service
endif

reformed_church: guilds divine_right root
ifeq (,$(wildcard ./makefiles/reformed_church))
	@echo reformed_church
	@touch ./makefiles/reformed_church
endif

the_enlightenment: humanism diplomatic_service root
ifeq (,$(wildcard ./makefiles/the_enlightenment))
	@echo the_enlightenment
	@touch ./makefiles/the_enlightenment
endif

civil_engineering: mercantilism root
ifeq (,$(wildcard ./makefiles/civil_engineering))
	@echo civil_engineering
	@touch ./makefiles/civil_engineering
endif

nationalism: the_enlightenment root
ifeq (,$(wildcard ./makefiles/nationalism))
	@echo nationalism
	@touch ./makefiles/nationalism
endif

opera_ballet: the_enlightenment root
ifeq (,$(wildcard ./makefiles/opera_ballet))
	@echo opera_ballet
	@touch ./makefiles/opera_ballet
endif

natural_history: colonialism root
ifeq (,$(wildcard ./makefiles/natural_history))
	@echo natural_history
	@touch ./makefiles/natural_history
endif

urbanization: civil_engineering nationalism root
ifeq (,$(wildcard ./makefiles/urbanization))
	@echo urbanization
	@touch ./makefiles/urbanization
endif

conservation: natural_history urbanization root
ifeq (,$(wildcard ./makefiles/conservation))
	@echo conservation
	@touch ./makefiles/conservation
endif

mobilization: urbanization root
ifeq (,$(wildcard ./makefiles/mobilization))
	@echo mobilization
	@touch ./makefiles/mobilization
endif

cultural_heritage: conservation root
ifeq (,$(wildcard ./makefiles/cultural_heritage))
	@echo cultural_heritage
	@touch ./makefiles/cultural_heritage
endif

ideology: mass_media mobilization root
ifeq (,$(wildcard ./makefiles/ideology))
	@echo ideology
	@touch ./makefiles/ideology
endif

professional_sports: ideology root
ifeq (,$(wildcard ./makefiles/professional_sports))
	@echo professional_sports
	@touch ./makefiles/professional_sports
endif

