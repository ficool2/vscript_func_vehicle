// by ficool2

function OnPostSpawn()
{
	self.KeyValueFromString("classname", "func_vehicle_controls");
	
	local vehicle = self.GetMoveParent();
	if (!vehicle || vehicle.GetClassname() != "func_vehicle")
	{
		error(format("func_vehicle control '%s' at (%s) is not parented to a vehicle!\n", 
			self.GetName(), self.GetOrigin().ToKVString()));
		vehicle = null;
		return;
	}
	
	self.ConnectOutput("OnStartTouch", "OnStartTouch");
	self.ConnectOutput("OnEndTouch", "OnEndTouch");
	
	// prevent blocking the controls with buildings	
	local nobuild = SpawnEntityFromTable("func_nobuild",
	{
		model  = self.GetModelName(),
		origin = self.GetOrigin(),
		angles = self.GetAbsAngles()
	});
	nobuild.AcceptInput("SetParent", "!activator", self, self);
}

function OnStartTouch()
{
	if (activator)
		activator.GetScriptScope().funcvehicle_data.vehicle_controls = self;
}

function OnEndTouch()
{
	if (activator)
		activator.GetScriptScope().funcvehicle_data.vehicle_controls = null;
}