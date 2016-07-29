{application, ensafen, [
	{description, "Make a web endpoint safer by protecting it with this reverse proxy."},
	{vsn, "0.0.3"},
	{modules, ['ensafen_app','ensafen_sup','safify_handler']},
	{registered, [ensafen_sup]},
	{applications, [kernel,stdlib,cowboy,gun]},
	{mod, {ensafen_app, []}}
]}.
