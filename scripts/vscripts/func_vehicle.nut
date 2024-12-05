// by ficool2
// func_vehicle is a right, not a privilege

::ROOT <- getroottable();
if (!("FuncVehicleInit" in ROOT))
{
	const MOVETYPE_STEP = 3;
	const MASK_PLAYERSOLID_BRUSHONLY = 81931; //CONTENTS_SOLID|CONTENTS_MOVEABLE|CONTENTS_WINDOW|CONTENTS_PLAYERCLIP|CONTENTS_GRATE
	const MASK_PLAYERSOLID_BRUSHONLY_WATER = 81963; // CONTENTS_SOLID|CONTENTS_MOVEABLE|CONTENTS_WINDOW|CONTENTS_PLAYERCLIP|CONTENTS_GRATE|CONTENTS_WATER
	const SOLID_NONE = 0;
	const SOLID_BSP = 1;
	const SOLID_BBOX = 2;
	const FSOLID_NOT_SOLID = 4;
	const FSOLID_TRIGGER = 8;
	const SURF_WARP = 8;
	const IN_JUMP = 2;
	const IN_FORWARD = 8;
	const IN_BACK = 16;
	const IN_FORWARD_BACK = 24; // IN_FORWARD|IN_BACK
	const IN_USE = 32;
	const IN_MOVELEFT = 512;
	const IN_MOVERIGHT = 1024;
	const FL_ATCONTROLS = 128;
	const EF_NOSHADOW = 16;
	const EF_NORECEIVESHADOW = 64;
	const DEFAULT_TICKDT = 0.015;
	const CHAN_ITEM = 3;
	const CHAN_STATIC = 6;
	const SND_CHANGE_VOL = 1;
	const SND_CHANGE_PITCH = 2;
	const SND_STOP = 4;
	const SNDLVL_NORM = 75; // 0.8 attn
	const RECIPIENT_FILTER_GLOBAL = 5;
	const VEHICLE_SPAWNFLAGS = 642; // 2|128|512 no user control, HL1 train, unblockable by player
	const DMG_VEHICLE = 16;
	const DMG_VEHICLE_CRIT = 1048592; // DMG_VEHICLE|DMG_ACID
	const kRenderNone = 10;
	
	const DEG2RAD = 0.0174532924; // PI/180.0
	const M_00 = 00; const M_01 = 01; const M_02 = 02; const M_03 = 03;
	const M_10 = 04; const M_11 = 05; const M_12 = 06; const M_13 = 07;
	const M_20 = 08; const M_21 = 09; const M_22 = 10; const M_23 = 11;
	const M_30 = 12; const M_31 = 13; const M_32 = 14; const M_33 = 15;

	::FUNCVEHICLE_PLAYERDATA <- class
	{
		constructor(_player)
		{
			player = _player;
			next_use_time = 0.0;
			jumped = false;
		}
		
		player = null;
		vehicle = null;
		vehicle_scope = null;
		vehicle_controls = null;
		buttons = null;
		origin = null;
		mins = null;
		maxs = null;
		world_mins = null;
		world_maxs = null;
		ground = null;
		next_use_time = null;
		jumped = null;
	};
	
	::FUNCVEHICLE_BUILDINGDATA <- class
	{
		constructor (_building)
		{
			building = _building;
			origin = _building.GetOrigin();
			mins = _building.GetBoundingMins();
			maxs = _building.GetBoundingMaxs();
			world_mins = origin + mins;
			world_maxs = origin + maxs;
		}
		
		building = null;
		origin = null;
		mins = null;
		maxs = null;
		world_mins = null;
		world_maxs = null;
	};
	
	::FuncVehicles <- [];
	::FuncVehicleScopes <- [];
	::FuncVehiclePlayers <- [];
	::FuncVehiclePlayersData <- [];
	::FuncVehicleBuildingsData <- [];
	::FuncVehicleBuildingDataRemoveQueue <- [];
	::FuncVehicleTime <- 0.0;
	::FuncVehicleDispenserID <- 0;
	
	::VectorAngles <- function(forward)
	{
		local yaw, pitch;
		if (forward.y == 0.0 && forward.x == 0.0)
		{
			yaw = 0.0;
			if (forward.z > 0.0)
				pitch = 270.0;
			else
				pitch = 90.0;
		}
		else
		{
			yaw = (atan2(forward.y, forward.x) * 180.0 / PI);
			if (yaw < 0.0)
				yaw += 360.0;
			pitch = (atan2(-forward.z, forward.Length2D()) * 180.0 / PI);
			if (pitch < 0.0)
				pitch += 360.0;
		}

		return QAngle(pitch, yaw, 0.0);
	}

	::AngleNormalize <- function(target)
	{
		target %= 360.0;
		if (target > 180.0)
			target -= 360.0;
		else if (target < -180.0)
			target += 360.0;
		return target;
	}

	::AnglesFixup <- function(angles)
	{
		return QAngle(AngleNormalize(angles.x), AngleNormalize(angles.y), AngleNormalize(angles.z));
	}

	::AngleDistance <- function(next, cur)
	{
		local delta = next - cur;
		if (delta < -180.0)
			delta += 360.0;
		else if (delta > 180.0)
			delta -= 360.0;
		return delta;
	}

	::Matrix4x4_Init <- function(origin, angles)
	{
		local out = array(16);
		
		local angle, sr, sp, sy, cr, cp, cy;
		if (angles.z != 0.0)
		{	
			angle = angles.y * DEG2RAD;
			sy = sin(angle);
			cy = cos(angle);
			angle = angles.x * DEG2RAD;
			sp = sin(angle);
			cp = cos(angle);
			angle = angles.z * DEG2RAD;
			sr = sin(angle);
			cr = cos(angle);		

			out[M_00] = cp * cy;
			out[M_01] = sr * sp * cy + cr * -sy;
			out[M_02] = cr * sp * cy + -sr * -sy;
			out[M_03] = origin.x;
			out[M_10] = cp * sy;
			out[M_11] = sr * sp * sy + cr * cy;
			out[M_12] = cr * sp * sy +- sr * cy;
			out[M_13] = origin.y;
			out[M_20] = -sp;
			out[M_21] = sr * cp;
			out[M_22] = cr * cp;
			out[M_23] = origin.z;
			out[M_30] = 0.0;
			out[M_31] = 0.0;
			out[M_32] = 0.0;
			out[M_33] = 1.0;
		}
		else if (angles.x != 0.0)
		{
			angle = angles.y * DEG2RAD;
			sy = sin(angle);
			cy = cos(angle);
			angle = angles.x * DEG2RAD;
			sp = sin(angle);
			cp = cos(angle);

			out[M_00] = cp * cy;
			out[M_01] = -sy;
			out[M_02] = sp * cy;
			out[M_03] = origin.x;
			out[M_10] = cp * sy;
			out[M_11] = cy;
			out[M_12] = sp * sy;
			out[M_13] = origin.y;
			out[M_20] = -sp;
			out[M_21] = 0.0;
			out[M_22] = cp;
			out[M_23] = origin.z;
			out[M_30] = 0.0;
			out[M_31] = 0.0;
			out[M_32] = 0.0;
			out[M_33] = 1.0;
		}
		else if (angles.y != 0.0)
		{
			angle = angles.y * DEG2RAD;
			sy = sin(angle);
			cy = cos(angle);

			out[M_00] = cy;
			out[M_01] = -sy;
			out[M_02] = 0.0;
			out[M_03] = origin.x;
			out[M_10] = sy;
			out[M_11] = cy;
			out[M_12] = 0.0;
			out[M_13] = origin.y;
			out[M_20] = 0.0;
			out[M_21] = 0.0;
			out[M_22] = 1.0;
			out[M_23] = origin.z;
			out[M_30] = 0.0;
			out[M_31] = 0.0;
			out[M_32] = 0.0;
			out[M_33] = 1.0;
		}
		else
		{
			out[M_00] = 1.0;
			out[M_01] = 0.0;
			out[M_02] = 0.0;
			out[M_03] = origin.x;
			out[M_10] = 0.0;
			out[M_11] = 1.0;
			out[M_12] = 0.0;
			out[M_13] = origin.y;
			out[M_20] = 0.0;
			out[M_21] = 0.0;
			out[M_22] = 1.0;
			out[M_23] = origin.z;
			out[M_30] = 0.0;
			out[M_31] = 0.0;
			out[M_32] = 0.0;
			out[M_33] = 1.0;
		}
		
		return out;
	}

	::VectorTransform <- function(m, v)
	{
		return Vector
		(
			v.x * m[M_00] + v.y * m[M_01] + v.z * m[M_02] + m[M_03],
			v.x * m[M_10] + v.y * m[M_11] + v.z * m[M_12] + m[M_13],
			v.x * m[M_20] + v.y * m[M_21] + v.z * m[M_22] + m[M_23]
		);
	}

	::VectorITransform <- function(m, v)
	{
		local dir = Vector
		(
			v.x - m[M_03],
			v.y - m[M_13],
			v.z - m[M_23]
		);
		return Vector
		(
			dir.x * m[M_00] + dir.y * m[M_10] + dir.z * m[M_20],
			dir.x * m[M_01] + dir.y * m[M_11] + dir.z * m[M_21],
			dir.x * m[M_02] + dir.y * m[M_12] + dir.z * m[M_22]
		);
	}

	::AABBTransform_Mins <- null;
	::AABBTransform_Maxs <- null;
	::AABBTransform <- function(m, mins, maxs)
	{
		local local_center = (mins + maxs) * 0.5;
		local local_extents = maxs - local_center;
		local world_center = VectorTransform(m, local_center);
		
		local world_extents = Vector
		(
			fabs(local_extents.x * m[M_00]) +
			fabs(local_extents.y * m[M_01]) +
			fabs(local_extents.z * m[M_02]),
			fabs(local_extents.x * m[M_10]) +
			fabs(local_extents.y * m[M_11]) +
			fabs(local_extents.z * m[M_12]),
			fabs(local_extents.x * m[M_20]) +
			fabs(local_extents.y * m[M_21]) +
			fabs(local_extents.z * m[M_22]) 
		);
		
		AABBTransform_Mins = world_center - world_extents;
		AABBTransform_Maxs = world_center + world_extents;
	}
	
	::FuncVehicleInit <- true;
}

function FuncVehicle(params)
{
	const_width        <- 0.0;
	const_length       <- 0.0;
	const_height       <- 0.0;
	const_speed        <- 165.0;
	const_acceleration <- 5.0;
	const_sounds       <- 3;
	const_dmg          <- 100.0;
	const_vphysics     <- true;
	foreach (param in ["width", "length", "height", "speed", "acceleration", "sounds", "dmg", "vphysics"])
	{
		if (param in params);
			this["const_" + param] = params[param];
	}
	
	vehicle <- self;
	collider <- null;
	driver <- null;
	driver_data <- null;
	
	speed <- 0.0;
	volume <- 1.0;
	noise <- null;
	turn_angle <- 0;
	turn_start_time <- -1.0;
	steering_wheel_decay <- 0.0;
	accelerator_decay <- 0.0;
	launch_time <- -1.0;
	can_turn_time <- 0.0;
	update_sound_time <- -1.0;
	is_sound_playing <- false;
	has_accelerate_decay <- false;
	hud_update_time <- 0.0;
	
	gravity <- null;
	vforward <- null;
	vright <- null;
	vup <- null;
	front_left <- null;
	front_right <- null;
	front <- null;
	back_left <- null;
	back_right <- null;
	back <- null;
	surface_normal <- Vector(0, 0, 1);
	last_contact_normal <- Vector(0, 0, 1);
	vehicle_direction <- Vector();
	origin <- null;
	angles <- null;
	forward <- null;
	right <- null;
	up <- null;
	velocity <- Vector();
	avelocity <- QAngle();
	mins <- null;
	maxs <- null;
	return this;
}

function Precache()
{
	if (const_sounds == 0)
		const_sounds = 3;
	
	switch (const_sounds)
	{
		case 1: PrecacheSound("plats/vehicle1.wav"); noise = "plats/vehicle1.wav"; break;
		case 2: PrecacheSound("plats/vehicle2.wav"); noise = "plats/vehicle2.wav"; break;
		case 3: PrecacheSound("plats/vehicle3.wav"); noise = "plats/vehicle3.wav"; break;
		case 4: PrecacheSound("plats/vehicle4.wav"); noise = "plats/vehicle4.wav"; 	;
		case 5: PrecacheSound("plats/vehicle6.wav"); noise = "plats/vehicle6.wav"; break;
		case 6: PrecacheSound("plats/vehicle7.wav"); noise = "plats/vehicle7.wav"; break;
	}
	
	PrecacheSound("plats/vehicle_brake1.wav");
	PrecacheSound("plats/vehicle_ignition.wav");
	
	local spawnflags = NetProps.GetPropInt(self, "m_spawnflags");
	spawnflags = spawnflags | VEHICLE_SPAWNFLAGS; // mandatory spawnflags
	NetProps.SetPropInt(self, "m_spawnflags", spawnflags);
	
	self.KeyValueFromString("classname", "func_vehicle");	
}

function OnPostSpawn()
{
	NetProps.SetPropInt(self, "m_nNextThinkTick", 0x7FFFFFFF); // don't use base train think
	NetProps.SetPropFloat(self, "m_flMoveDoneTime", 1e30); // enables physics
	self.SetMoveType(MOVETYPE_STEP, 0); // override MOVETYPE_PUSH, required for vphysics interactions
	
	FuncVehicles.append(self);
	FuncVehicleScopes.append(this);
	
	mins = self.GetBoundingMins();
	maxs = self.GetBoundingMaxs();
	
	if (const_vphysics)
	{	
		// hack that allows ragdolls to collide with the vehicle clientside, for fun
		collider = SpawnEntityFromTable("prop_door_rotating",
		{
			classname  = "func_vehicle_collider",
			model      = "models/empty.mdl" // must set a mdl or crash
			origin     = self.GetOrigin()
			angles     = self.GetAbsAngles()
			spawnflags = 36864
		});
		collider.SetSolid(SOLID_NONE);
		collider.AddSolidFlags(FSOLID_NOT_SOLID);
		NetProps.SetPropInt(collider, "m_nRenderMode", kRenderNone);
		collider.SetModel(self.GetModelName());
		// can't parent it because physics doesn't update
	}
	
	// forcefully disable shadows on vehicles to prevent client crashes
	local effects = NetProps.GetPropInt(self, "m_fEffects");
	effects = effects | (EF_NOSHADOW|EF_NORECEIVESHADOW);
	NetProps.SetPropInt(self, "m_fEffects", effects);

	self.SetAbsAngles(QAngle(0.0, NetProps.GetPropFloat(self, "m_flBank"), 0.0));
}

function Trace(_start, _end)
{
	local trace = 
	{
		start = _start,
		end = _end,
		mask = MASK_PLAYERSOLID_BRUSHONLY,
		ignore = vehicle,
		startsolid = false,
	};
	
	TraceLineEx(trace);
	return trace;
}

function Use()
{	
	// ficool2: bootleg hud	
	local hud_state;
	local speed_ratio = speed / const_speed;
	if (speed_ratio > 0.95)
		hud_state = "4";
	else if (speed_ratio > 0.5)
		hud_state = "3";	
	else if (speed_ratio > 0.1)
		hud_state = "2";
	else if (speed_ratio < -0.0)
		hud_state = "R";
	else
		hud_state = "1";
	if (hud_update_time < FuncVehicleTime)
	{
		hud_update_time = FuncVehicleTime + 0.2;
		FuncVehicleText.KeyValueFromString("message", "Gear: " + hud_state);
		FuncVehicleText.AcceptInput("Display", "", driver, driver);	
	}
	
	local buttons = driver_data.buttons;
	if (buttons == 0)
		return;
	
	local movement_buttons = buttons & IN_FORWARD_BACK;
	if (movement_buttons > 0 && movement_buttons != IN_FORWARD_BACK) // don't move if both are held at same time
	{
		local cur_speed_ratio = 1.0; // ficool2: I'm not sure what value this is supposed to be initially
		if (buttons & IN_FORWARD)
		{
			cur_speed_ratio = speed_ratio;

			if (speed < 0)			cur_speed_ratio = cur_speed_ratio + (0.015/3)  + const_acceleration * 0.0005;
			else if (speed < 10)	cur_speed_ratio = cur_speed_ratio + (0.015/7)  + const_acceleration * 0.0006;
			else if (speed < 20)	cur_speed_ratio = cur_speed_ratio + (0.02/6)   + const_acceleration * 0.0007;
			else if (speed < 30)	cur_speed_ratio = cur_speed_ratio + (0.025/6)  + const_acceleration * 0.0007;
			else if (speed < 45)	cur_speed_ratio = cur_speed_ratio + (0.02/5)   + const_acceleration * 0.0007;
			else if (speed < 60)	cur_speed_ratio = cur_speed_ratio + (0.019/5)  + const_acceleration * 0.0008;
			else if (speed < 80)	cur_speed_ratio = cur_speed_ratio + (0.018/4)  + const_acceleration * 0.0008;
			else if (speed < 100)	cur_speed_ratio = cur_speed_ratio + (0.017/4)  + const_acceleration * 0.0009;
			else if (speed < 150)	cur_speed_ratio = cur_speed_ratio + (0.016/6)  + const_acceleration * 0.0008;
			else if (speed < 225)	cur_speed_ratio = cur_speed_ratio + (0.016/7)  + const_acceleration * 0.0007;
			else if (speed < 300)	cur_speed_ratio = cur_speed_ratio + (0.015/8)  + const_acceleration * 0.0006;
			else if (speed < 400)	cur_speed_ratio = cur_speed_ratio + (0.013/9)  + const_acceleration * 0.0005;
			else if (speed < 550)	cur_speed_ratio = cur_speed_ratio + (0.012/10) + const_acceleration * 0.0005;
			else if (speed < 800)	cur_speed_ratio = cur_speed_ratio + (0.011/12) + const_acceleration * 0.0005;
		}
		else if (buttons & IN_BACK)
		{
			cur_speed_ratio = speed_ratio;

			if (cur_speed_ratio > 0)									 cur_speed_ratio = cur_speed_ratio - 0.0125;
			else if (cur_speed_ratio <= 0 && cur_speed_ratio > -0.05)	 cur_speed_ratio = cur_speed_ratio - 0.0075;
			else if (cur_speed_ratio <= 0.05 && cur_speed_ratio > -0.1)	 cur_speed_ratio = cur_speed_ratio - 0.01;
			else if (cur_speed_ratio <= 0.15 && cur_speed_ratio > -0.15) cur_speed_ratio = cur_speed_ratio - 0.0125;
			else if (cur_speed_ratio <= 0.15 && cur_speed_ratio > -0.22) cur_speed_ratio = cur_speed_ratio - 0.01375;
			else if (cur_speed_ratio <= 0.22 && cur_speed_ratio > -0.3)	 cur_speed_ratio = cur_speed_ratio - 0.0175;
			else if (cur_speed_ratio <= 0.3)							 cur_speed_ratio = cur_speed_ratio - 0.0125;
		}

		if (cur_speed_ratio > 1)
		{
			cur_speed_ratio = 1;
		}
		else if (cur_speed_ratio < -0.35)
		{
			cur_speed_ratio = -0.35;
		}
		
		speed = cur_speed_ratio * const_speed;
		has_accelerate_decay = true;
	}
	// ficool2: not sure why this was an "else if" originally
	/*else*/ if (can_turn_time < FuncVehicleTime)
	{
		// ficool2: this is the wrong way to do it...
		local buttonleft, buttonright;
		if (buttons & IN_BACK)
		{
			buttonleft = IN_MOVERIGHT;
			buttonright = IN_MOVELEFT;
		}
		else
		{
			buttonleft = IN_MOVELEFT;
			buttonright = IN_MOVERIGHT;
		}
		
		if (buttons & buttonleft)
		{
			turn_angle++;
			steering_wheel_decay = FuncVehicleTime + 0.075;

			if (turn_angle > 8)
			{
				turn_angle = 8;
			}
		}
		else if (buttons & buttonright)
		{
			turn_angle--;
			steering_wheel_decay = FuncVehicleTime + 0.075;

			if (turn_angle < -8)
			{
				turn_angle = -8;
			}
		}
	
		can_turn_time = FuncVehicleTime + 0.05;
	}
}

function CheckTurning()
{
	local maxspeed;
	local tr;
		
	if (turn_angle < 0)
	{
		if (speed > 0)
		{
			tr = Trace(front_right, front_right - (right * 16.0));
		}
		else if (speed < 0)
		{
			tr = Trace(back_left, back_left + (right * 16.0));
		}

		if (tr && tr.fraction != 1.0)
		{
			turn_angle = 1;
		}
	}
	else if (turn_angle > 0)
	{
		if (speed > 0)
		{
			tr = Trace(front_left, front_left + (right * 16.0));
		}
		else if (speed < 0)
		{
			tr = Trace(back_right, back_right - (right * 16.0));
		}

		if (tr && tr.fraction != 1.0)
		{
			turn_angle = -1;
		}
	}

	if (speed > 0)
	{
		local turn_counter = abs(turn_angle);

		if (turn_counter > 4)
		{
			if (turn_start_time != -1)
			{
				local turn_time = FuncVehicleTime - turn_start_time;

				if (turn_time >= 0)			maxspeed = const_speed * 0.98;
				else if (turn_time > 0.3)	maxspeed = const_speed * 0.95;
				else if (turn_time > 0.6)	maxspeed = const_speed * 0.9;
				else if (turn_time > 0.8)	maxspeed = const_speed * 0.8;
				else if (turn_time > 1)		maxspeed = const_speed * 0.7;
				else if (turn_time > 1.2)	maxspeed = const_speed * 0.5;
				else						maxspeed = turn_time;
			}
			else
			{
				turn_start_time = FuncVehicleTime;
				maxspeed = const_speed;
			}
		}
		else
		{
			turn_start_time = -1;

			if (turn_counter > 2)
				maxspeed = const_speed * 0.9;
			else
				maxspeed = const_speed;
		}

		if (maxspeed < speed)
		{
			speed -= const_speed * 0.1;
		}
	}
}

function TerrainFollowing()
{
	// ficool2: if the vehicle is falling, don't try to follow slopes
	// this fixes vehicles floating after driving off an edge
	local follow_dist = launch_time < 0.0 ? 48.0 : 0.0;
	local tr = 
	{
		start = origin,
		end = origin + Vector(0, 0, -(const_height + follow_dist)),
		mask = MASK_PLAYERSOLID_BRUSHONLY_WATER,
		ignore = vehicle
	};
	TraceLineEx(tr);
	
	if (tr.fraction != 1.0) 
	{
		if (tr.surface_flags & SURF_WARP) // water
		{
			tr.mask = MASK_PLAYERSOLID_BRUSHONLY;
			TraceLineEx(tr);
			
			if (tr.fraction != 1.0)
				surface_normal = tr.plane_normal;
			else
				surface_normal = Vector(0, 0, 1); // makes vehicle float in water
		}
		else
		{
			surface_normal = tr.plane_normal;		
		}	
	}
	else
	{
		surface_normal = Vector();
	}
	
	last_contact_normal = surface_normal + Vector();
}

function CollisionDetection()
{
	local tr;
	
	if (speed < 0)
	{
		tr = Trace(back_left, back_left + (forward * 16.0));

		if (tr.fraction == 1.0)
		{
			tr = Trace(back_right, back_right + (forward * 16.0));

			if (tr.fraction == 1.0)
			{
				tr = Trace(back, back + (forward * 16.0));

				if (tr.fraction == 1.0)
				{
					return;
				}
			}

			if (forward.Dot(tr.plane_normal * -1.0) < 0.7 && tr.plane_normal.z < 0.1)
			{
				surface_normal = tr.plane_normal;
				surface_normal.z = 0;

				speed *= 0.99;
			}
			else if (tr.plane_normal.z < 0.65 || tr.startsolid)
			{
				speed *= -1.0;
			}
			else
			{
				surface_normal = tr.plane_normal;
			}
		}
		else
		{
			if (forward.Dot(tr.plane_normal * -1.0) < 0.7 && tr.plane_normal.z < 0.1)
			{
				surface_normal = tr.plane_normal;
				surface_normal.z = 0;

				speed *= 0.99;
			}
			else if (tr.plane_normal.z < 0.65 || tr.startsolid)
			{
				speed *= -1.0;
			}
			else
			{
				surface_normal = tr.plane_normal;
			}
		}
	}
	else if (speed > 0)
	{
		tr = Trace(front_left, front_left - (forward * 16.0));

		if (tr.fraction == 1.0)
		{
			tr = Trace(front_right, front_right - (forward * 16.0));

			if (tr.fraction == 1.0)
			{
				tr = Trace(front, front - (forward * 16.0));

				if (tr.fraction == 1.0)
				{
					return;
				}
			}
		}

		if (forward.Dot(tr.plane_normal * -1.0) > -0.7 && tr.plane_normal.z < 0.1)
		{
			surface_normal = tr.plane_normal;
			surface_normal.z = 0;

			speed *= 0.99;
		}
		else if (tr.plane_normal.z < 0.65 || tr.startsolid)
		{
			speed *= -1.0;
		}
		else
		{
			surface_normal = tr.plane_normal;
		}
	}
}

function PushRotateMove(velocity, avelocity)
{
	local transform = Matrix4x4_Init(origin, angles);
	local new_origin = origin + velocity * DEFAULT_TICKDT;
	local new_angles = angles + avelocity * DEFAULT_TICKDT;
	local new_transform = Matrix4x4_Init(new_origin, new_angles);
	
	vehicle.SetLocalOrigin(new_origin);
	vehicle.SetLocalAngles(new_angles);
	
	if (collider)
	{
		collider.SetAbsOrigin(new_origin);
		collider.SetAbsAngles(new_angles);
	}

	// GetBoundingMinsMaxOriented doesn't transform the AABB correctly.. sigh
	AABBTransform(new_transform, mins, maxs);
	// avoid 6 table accesses each iteration
	local pusher_mins_x = AABBTransform_Mins.x;
	local pusher_mins_y = AABBTransform_Mins.y;
	local pusher_mins_z = AABBTransform_Mins.z;
	local pusher_maxs_x = AABBTransform_Maxs.x;
	local pusher_maxs_y = AABBTransform_Maxs.y;
	local pusher_maxs_z = AABBTransform_Maxs.z;
	
	forward = new_angles.Forward();
	right = new_angles.Left();
	up = new_angles.Up();

	local nudge_dirs;
	
	// try up direction first as thats usually where stuck cases happen
	// for tilted vehicles, use much more aggressive nudging so passengers dont tend to die
	if (new_angles.x == 0.0)
		nudge_dirs = [up, forward, forward * -1.0, right, right * -1.0];
	else
		nudge_dirs = [up * 2.0, forward * 1.5, forward * -1.5, right * 1.5, right * -1.5,
					  up * 5.0, forward * 5.0, forward * -5.0, right * 5.0, right * -5.0];
		
	foreach (data in FuncVehiclePlayersData)
	{
		local check_world_mins = data.world_mins;
		local check_world_maxs = data.world_maxs;
		
		if ((check_world_mins.x > pusher_maxs_x) || (check_world_maxs.x < pusher_mins_x))
			continue;
		if ((check_world_mins.y > pusher_maxs_y) || (check_world_maxs.y < pusher_mins_y))
			continue;
		if ((check_world_mins.z > pusher_maxs_z) || (check_world_maxs.z < pusher_mins_z))
			continue;
			
		local check_mins = data.mins;
		local check_maxs = data.maxs;
		
		local check = data.player;
		local check_origin = data.origin;
		local tr;
		
		if (data.ground != vehicle)
		{	
			tr =
			{
				start = check_origin,
				end = check_origin,
				hullmin = check_mins,
				hullmax = check_maxs,
				mask = MASK_PLAYERSOLID_BRUSHONLY,
				ignore = check
			};
			TraceHull(tr);
			
			if (!tr.hit || tr.enthit != vehicle)
				continue;
		}

		local check_new_origin = check_origin + (VectorTransform(new_transform, VectorITransform(transform, check_origin)) - check_origin);
		
		// reuse previous trace table since only "hit" is checked which always gets overriden
		if (tr)
		{
			tr.start = check_new_origin;
			tr.end = check_new_origin;
		}
		else
		{
			tr = 
			{
				start = check_new_origin,
				end = check_new_origin,
				hullmin = check_mins,
				hullmax = check_maxs,
				mask = MASK_PLAYERSOLID_BRUSHONLY,
				ignore = check
			};
		}
		TraceHull(tr);
		
		if (tr.hit)
		{
			local blocked = true;
			
			foreach (nudge_dir in nudge_dirs)
			{
				local nudge_origin = check_new_origin + nudge_dir;
				tr.start = nudge_origin;
				tr.end = nudge_origin;
				TraceHull(tr);

				if (tr.hit)
					continue;

				check_new_origin = nudge_origin;
				blocked = false;
				break;
			}
				
			if (blocked)
			{				
				// better to let them get stuck in the vehicle than in the map
				BlockedPlayer(check, data);
				continue;
			}
		}
		
		check.SetAbsOrigin(check_new_origin);
	}
	
	foreach (data in FuncVehicleBuildingsData)
	{
		local check_world_mins = data.world_mins;
		local check_world_maxs = data.world_maxs;
		
		if ((check_world_mins.x > pusher_maxs_x) || (check_world_maxs.x < pusher_mins_x))
			continue;
		if ((check_world_mins.y > pusher_maxs_y) || (check_world_maxs.y < pusher_mins_y))
			continue;
		if ((check_world_mins.z > pusher_maxs_z) || (check_world_maxs.z < pusher_mins_z))
			continue;

		local check = data.building;
		local check_origin = data.origin;
		local tr = 
		{
			start = check_origin,
			end = check_origin,
			hullmin = data.mins,
			hullmax = data.maxs,
			mask = MASK_PLAYERSOLID_BRUSHONLY,
			ignore = check
		};
		TraceHull(tr);
		
		if (tr.hit)
		{
			if (tr.enthit == vehicle)
				BlockedBuilding(check, data);
		}
	}
}

function UpdateSound()
{
	if (noise)
	{
		local pitch = 60 + (abs(speed) * (200 - 60) / 1500);
		if (pitch > 200)
			pitch = 200;
			
		if (!is_sound_playing)
		{
			if (const_sounds < 5)
			{
				EmitSoundEx({
					sound_name = "plats/vehicle_brake1.wav",
					channel = CHAN_ITEM,
					volume = volume,
					sound_level = SNDLVL_NORM,
					entity = vehicle
				});				
			}
								
			EmitSoundEx({
				sound_name = noise,
				channel = CHAN_STATIC,
				volume = volume,
				pitch = pitch,
				sound_level = SNDLVL_NORM,
				entity = vehicle,
				filter_type = RECIPIENT_FILTER_GLOBAL // so players outside range can hear it
			});					
			
			is_sound_playing = true;
		}
		else
		{
			EmitSoundEx({
				sound_name = noise,
				channel = CHAN_STATIC,
				volume = volume,
				pitch = pitch,
				flags = SND_CHANGE_PITCH,
				sound_level = SNDLVL_NORM,
				entity = vehicle,
				filter_type = RECIPIENT_FILTER_GLOBAL // so players outside range can hear it
			});
		}
	}
}

function Update()
{
	// ficool2: don't do expensive calculations when stopped with no driver
	if (speed == 0.0 && !driver_data)
		return 0.2;
	
	origin = vehicle.GetOrigin();
	angles = vehicle.GetLocalAngles();
	forward = angles.Forward();
	right = angles.Left();
	up = angles.Up();
	
	vforward = (forward * -1.0) * (const_width * 0.5);
	vright = (right * -1.0) * (const_length * 0.5);
	vup = up * 16.0;
	
	front_left = origin + vforward - vright + vup;
	front_right = origin + vforward + vright + vup;
	front = origin + vforward + vup;
	back_left = origin - vforward - vright + vup;
	back_right = origin - vforward + vright + vup;
	back = origin - vforward + vup;
	
	if (driver_data)
	{
		if (driver_data.vehicle_controls)
			Use();
		else
			Exit();
	}
	
	CheckTurning();
	
	if (steering_wheel_decay < FuncVehicleTime)
	{
		steering_wheel_decay = FuncVehicleTime + 0.1;

		if (turn_angle < 0)
			turn_angle++;
		else if (turn_angle > 0)
			turn_angle--;
	}

	if (accelerator_decay < FuncVehicleTime)
	{
		accelerator_decay = FuncVehicleTime + 0.1;

		if (speed < 0)
		{
			speed += 20;

			if (speed > 0)
				speed = 0;
		}
		else if (speed > 0)
		{
			speed -= 20;

			if (speed < 0)
				speed = 0;
		}
	}
	
	if (speed == 0.0)
	{
		turn_angle = 0;
		velocity = Vector();
		avelocity = QAngle();
		//vehicle.SetAbsVelocity(Vector());
		//vehicle.SetAngularVelocity(0, 0, 0);
		return -1;
	}
	
	TerrainFollowing();
	CollisionDetection();
	
	// ficool2: if collision detection resulted in a zero normal, try use the normal from initial contact
	// this fixes vehicles falling through the ground and slope jitter
	if (surface_normal.LengthSqr() == 0.0)
		surface_normal = last_contact_normal;
	
	if (surface_normal.LengthSqr() != 0.0)
	{
		vehicle_direction = surface_normal.Cross(forward);
		vehicle_direction = surface_normal.Cross(vehicle_direction);
		
		local target_angle = VectorAngles(vehicle_direction);
		target_angle.y += 180;

		if (turn_angle != 0)
		{
			target_angle.y += turn_angle;
		}

		target_angle = AnglesFixup(target_angle);
		local angle = AnglesFixup(angles);
		
		// ficool2: mysterious sign flip 
		angle.x *= -1.0;
		
		local vx = AngleDistance(target_angle.x, angle.x);
		local vy = AngleDistance(target_angle.y, angle.y);

		if (vx > 10)
			vx = 10;
		else if (vx < -10)
			vx = -10;

		if (vy > 10)
			vy = 10;
		else if (vy < -10)
			vy = -10;
			

		avelocity.y = (vy * 10).tointeger();
		avelocity.x = (vx * -10).tointeger(); // ficool2: mysterious sign flip

		launch_time = -1;
	}
	else
	{
		if (launch_time != -1)
		{
			gravity = Vector(0, 0, (FuncVehicleTime - launch_time) * -35); 
			if (gravity.z < -400)
			{
				gravity.z = -400;
			}
		}
		else
		{
			launch_time = FuncVehicleTime;
			gravity = Vector(0, 0, 0);
			velocity = velocity * 1.5;
		}

		vehicle_direction = forward * -1;
	}
	
	if (update_sound_time < FuncVehicleTime && driver)
	{
		UpdateSound();
		update_sound_time = FuncVehicleTime + 1.0;
	}
	
	if (surface_normal.LengthSqr() != 0.0)
	{
		//vehicle_direction.Norm();
		velocity = vehicle_direction * speed;
	}
	else
	{
		velocity = velocity + gravity;
		
	}
	
	if (has_accelerate_decay)
	{
		accelerator_decay = FuncVehicleTime + 0.25;
		has_accelerate_decay = false;
	}

	// PushRotateMove takes care of this now
	//vehicle.SetAbsVelocity(velocity);
	//vehicle.SetAngularVelocity(avelocity.x, avelocity.y, avelocity.z);
	
	PushRotateMove(velocity, avelocity);
	
	return -1;
}

function BlockedPlayer(blocker, blocker_data)
{
	if (blocker_data.ground == vehicle)
	{
		NetProps.SetPropVector(blocker, "m_vecBaseVelocity", velocity);
		return;
	}
	
	local blocker_velocity = blocker_data.origin - origin;
	blocker_velocity.Norm();
	blocker_velocity *= const_dmg;
	blocker_velocity.z += 300.0;
	blocker.SetAbsVelocity(blocker_velocity);
	velocity *= 0.85;
	
	// ficool2: original code had bounds checks here
	// but these shouldn't be necessary as the new push logic is more robust and less prone to stuck bugs
	blocker.TakeDamage(450.0, DMG_VEHICLE, vehicle);
}

function BlockedBuilding(blocker, blocker_data)
{
	if (driver)
	{
		// this hack allows friendlyfire against buildings
		local team = driver.GetTeam();
		NetProps.SetPropInt(driver, "m_iTeamNum", 0);
		// the validity check should NOT be necessary as the events should always keep track of valid buildings
		// however, if somehow that system breaks, this would error and leave a player on team unassigned (which will cause many, many bad things)
		// so don't let that happen
		if (blocker.IsValid())
			blocker.TakeDamage(4500.0, DMG_VEHICLE, vehicle);
		else
			FuncVehicleBuildingDataRemoveQueue.append(blocker_data);
		NetProps.SetPropInt(driver, "m_iTeamNum", team);
	}
	else
	{
		blocker.TakeDamage(4500.0, DMG_VEHICLE, vehicle);
	}
	
}

function Enter(player)
{
	if (driver)
		return false;
		
	EmitSoundEx({
		sound_name = "plats/vehicle_ignition.wav",
		channel = CHAN_ITEM,
		volume = 0.8,
		sound_level = SNDLVL_NORM,
		entity = player
	});	
	
	local scope = player.GetScriptScope();
		
	driver = player;
	driver.AddFlag(FL_ATCONTROLS);
	driver.AddCustomAttribute("no_jump", 1, -1);
	driver_data = scope.vehicle_data;
	driver_data.vehicle = vehicle;
	driver_data.vehicle_scope = this;
	
	turn_start_time = -1;
	update_sound_time = -1;
	return true;
}

function Exit()
{
	if (driver)
	{
		local data = driver.GetScriptScope().vehicle_data;
		data.vehicle = null;
		data.vehicle_scope = null;
		
		driver.RemoveFlag(FL_ATCONTROLS);
		driver.RemoveCustomAttribute("no_jump");
		driver = null;
		driver_data = null;
	}
	
	if (is_sound_playing && noise)
	{
		EmitSoundEx({
			sound_name = noise,
			channel = CHAN_STATIC,
			flags = SND_STOP,
			entity = vehicle,
			filter_type = RECIPIENT_FILTER_GLOBAL
		});
	}
	
	is_sound_playing = false;
}

function Delete()
{
	local idx = FuncVehicles.find(vehicle);
	if (idx != null)
	{
		FuncVehicles.remove(idx);
		FuncVehicleScopes.remove(idx);
	}
	
	Exit();
	vehicle.Destroy();
}

if (!("FuncVehicleEvents" in ROOT))
	::FuncVehicleEvents <- {};
::FuncVehicleEvents.clear();
::FuncVehicleEvents =
{
	OnGameEvent_player_spawn = function(params)
	{
		local player = GetPlayerFromUserID(params.userid);
		if (!player)
			return;
		
		if (params.team == 0) // unassigned
		{
			player.ValidateScriptScope();
			player.GetScriptScope().vehicle_data <- null;
			return;
		}
		
		player.ValidateScriptScope();		
		local scope = player.GetScriptScope();
		
		// in case player was respawned while alive
		local data = scope.vehicle_data;
		if (data && data.vehicle_scope)
			data.vehicle_scope.Exit();
		
		data = FUNCVEHICLE_PLAYERDATA(player);
		scope.vehicle_data = data;
		
		if (params.team & 2)
		{
			local idx = FuncVehiclePlayers.find(player);
			if (idx == null)
			{
				FuncVehiclePlayers.append(player);
				FuncVehiclePlayersData.append(data);
			}
			else
			{
				FuncVehiclePlayersData[idx] = data;
			}
		}
	}

	OnGameEvent_player_death = function(params)
	{
		local player = GetPlayerFromUserID(params.userid);
		if (!player)
			return;
			
		if (params.death_flags & 32) // dead ringer
			return;
			
		local idx = FuncVehiclePlayers.find(player);
		if (idx != null)
		{
			local data = FuncVehiclePlayersData[idx];
			if (data.vehicle_scope)
				data.vehicle_scope.Exit();
			
			FuncVehiclePlayers.remove(idx);
			FuncVehiclePlayersData.remove(idx);
		}
	}

	OnGameEvent_player_disconnect = function(params)
	{
		local player = GetPlayerFromUserID(params.userid);
		if (!player)
			return;
		
		local idx = FuncVehiclePlayers.find(player);
		if (idx != null)
		{
			local data = FuncVehiclePlayersData[idx];
			if (data.vehicle_scope)
				data.vehicle_scope.Exit();
			
			FuncVehiclePlayers.remove(idx);	
			FuncVehiclePlayersData.remove(idx);
		}
	}
	
	OnGameEvent_player_teleported = function(params)
	{
		local player = GetPlayerFromUserID(params.userid);
		if (!player)
			return;
	
		// fix tilted view from teleporters placed on vehicles
		local eye_ang = player.EyeAngles();
		if (eye_ang.z != 0.0)
		{
			eye_ang.z = 0.0;
			player.SnapEyeAngles(eye_ang);
		}
	}	
	
	OnGameEvent_player_builtobject = function(params)
	{
		local building = EntIndexToHScript(params.index);
		if (!building)
			return;
		
		// fix touch trigger on dispenser to be parented
		if (params.object == 0)
		{
			building.ValidateScriptScope();
			local scope = building.GetScriptScope();
			if ("touch_trigger" in scope && scope.touch_trigger.IsValid())
				scope.touch_trigger.Destroy();
				
			local touch_trigger_radius = 64;
			local touch_trigger = Entities.CreateByClassname("dispenser_touch_trigger");
			local touch_trigger_name = format("_trigger%d", FuncVehicleDispenserID++);
			touch_trigger.SetAbsOrigin(building.GetOrigin());
			touch_trigger.SetOwner(building);
			touch_trigger.DispatchSpawn();
			touch_trigger.SetSolid(SOLID_BBOX);
			touch_trigger.SetSize
			(
				Vector(-touch_trigger_radius, -touch_trigger_radius, -touch_trigger_radius),
				Vector(touch_trigger_radius, touch_trigger_radius, touch_trigger_radius)
			);
			touch_trigger.KeyValueFromString("targetname", touch_trigger_name);
			NetProps.SetPropBool(touch_trigger, "m_bForcePurgeFixedupStrings", true);
			NetProps.SetPropString(building, "m_iszCustomTouchTrigger", touch_trigger_name);
			touch_trigger.AcceptInput("SetParent", "!activator", building, building);
			
			scope.touch_trigger <- touch_trigger;
		}
		
		// ignore moving buildings
		local moveparent = building.GetMoveParent();
		if (moveparent != null)
			return;
			
		local building_origin = building.GetOrigin();
		local trace = 
		{
			start = building_origin + Vector(0, 0, 2),
			end = building_origin - Vector(0, 0, 2),
			mask = MASK_PLAYERSOLID_BRUSHONLY,
			ignore = building
		}
		TraceLineEx(trace);
		if (trace.hit && trace.enthit.GetClassname() == "func_vehicle")
		{
			building.AcceptInput("SetParent", "!activator", trace.enthit, trace.enthit);
			return;
		}
	
		building.ValidateScriptScope();
		local scope = building.GetScriptScope();
		if (!("data" in scope))
			scope.data <- FUNCVEHICLE_BUILDINGDATA(building);
		
		local idx = FuncVehicleBuildingsData.find(scope.data);
		if (idx == null)
			FuncVehicleBuildingsData.append(scope.data);
		else
			FuncVehicleBuildingsData[idx] = scope.data;
	}
	
	OnGameEvent_player_carryobject = function(params)
	{
		FuncVehicle_RemoveBuilding(EntIndexToHScript(params.index), false);
	}
	
	OnGameEvent_object_destroyed = function(params)
	{
		FuncVehicle_RemoveBuilding(EntIndexToHScript(params.index), true);
	}
	
	OnGameEvent_object_removed = function(params)
	{
		FuncVehicle_RemoveBuilding(EntIndexToHScript(params.index), true);
	}
	
	OnGameEvent_object_detonated = function(params)
	{
		FuncVehicle_RemoveBuilding(EntIndexToHScript(params.index), true);
	}	

	OnGameEvent_scorestats_accumulated_update = function(params)
	{
		foreach (vehicle_scope in FuncVehicleScopes)
			vehicle_scope.Exit();
			
		FuncVehicles.clear();
		FuncVehicleScopes.clear();
		FuncVehicleBuildingsData.clear();
	}

	OnScriptHook_OnTakeDamage = function(params)
	{
		local attacker = params.attacker;
		if (attacker && attacker.GetClassname() == "func_vehicle")
		{
			local scope = attacker.GetScriptScope();
			FuncVehicleProxy.SetAbsOrigin(scope.origin);
			
			// unfortunately this doesn't set the kill icon
			if (scope.velocity.Length() > 500.0)
				params.damage_type = DMG_VEHICLE_CRIT; // DMG_VEHICLE|DMG_ACID
			else
				params.damage_type = DMG_VEHICLE; // DMG_VEHICLE
	
			// make driver own the damage
			params.inflictor = FuncVehicleProxy;
			params.attacker = scope.driver;
		}
	}
}
__CollectGameEventCallbacks(FuncVehicleEvents)

::FuncVehicle_RemoveBuilding <- function(building, kill)
{
	if (!building)
		return;
		
	local scope = building.GetScriptScope();
	if (!scope)
		return;
		
	if (kill && "touch_trigger" in scope && scope.touch_trigger.IsValid())
		scope.touch_trigger.Destroy();
	
	if (!scope || !("data" in scope))
		return;
		
	local idx = FuncVehicleBuildingsData.find(scope.data);
	if (idx != null)	
		FuncVehicleBuildingsData.remove(idx);	
}

if (!("FuncVehicleText" in ROOT) || !FuncVehicleText.IsValid())
{
	::FuncVehicleText <- SpawnEntityFromTable("game_text",
	{
		classname = "func_vehicle_text"
		color     = "255 255 255"
		channel   = 0
		x         = 0.65
		y         = 0.94
		holdtime  = 0.4
		fadein    = 0
		fadeout   = 0
	})
}

// serves two purposes: as a kill icon, and as a global thinker
if (!("FuncVehicleProxy" in ROOT) || !FuncVehicleProxy.IsValid())
{
	::FuncVehicleProxy <- SpawnEntityFromTable("trigger_hurt", {}); // so uber doesn't protect
	FuncVehicleProxy.KeyValueFromString("classname", "vehicle");
	FuncVehicleProxy.RemoveSolidFlags(FSOLID_TRIGGER); // prevent console errors
	
	::FuncVehicleProxy_Think <- function()
	{
		// cache off data used for input + collision testing
		FuncVehicleTime = Time();
		
		local data, player, buttons, ground, parent;
		foreach (data in FuncVehiclePlayersData)
		{
			player = data.player;
			buttons = NetProps.GetPropInt(player, "m_nButtons");
			data.origin = player.GetOrigin();
			data.mins = player.GetPlayerMins();
			data.maxs = player.GetPlayerMaxs();
			data.world_mins = data.origin + data.mins;
			data.world_maxs = data.origin + data.maxs;
			data.buttons = buttons;
			
			// apply velocity to jumping off a vehicle
			if ((buttons & IN_JUMP) && NetProps.GetPropBool(player, "m_Shared.m_bJumping"))
			{
				if (!data.jumped)
				{
					ground = data.ground;
					if (ground && ground.GetClassname() == "func_vehicle")
						NetProps.SetPropVector(player, "m_vecBaseVelocity", ground.GetScriptScope().velocity);
					data.jumped = true;
				}
			}
			else
			{
				data.jumped = false;
			}
			
			// do this after the jump check because ground is gone after a jump
			ground = NetProps.GetPropEntity(player, "m_hGroundEntity");
			if (ground)
			{
				// if standing on a building parented to the vehicle, pretend they are touching the vehicle
				parent = ground.GetMoveParent();
				if (parent && parent.GetClassname() == "func_vehicle")
					ground = parent;
			}
			data.ground = ground;
			
			if (data.next_use_time < FuncVehicleTime)
			{
				if (data.vehicle_scope)
				{
					if ((buttons & IN_USE) || player.IsUsingActionSlot())
					{
						data.vehicle_scope.Exit();
						data.next_use_time = FuncVehicleTime + 1.0;			
					}
				}
				else
				{
					local controls = data.vehicle_controls;
					if (controls)
					{
						local vehicle_scope = controls.GetMoveParent().GetScriptScope();
						if (!vehicle_scope.driver && 
							((buttons & IN_USE) || player.IsUsingActionSlot()))
						{
							if (vehicle_scope.Enter(player))
								data.next_use_time = FuncVehicleTime + 1.0;
							else
								data.next_use_time = FuncVehicleTime + 0.2;
						}
					}
				}	
			}
		}
		
		// update every vehicle
		foreach (vehicle_scope in FuncVehicleScopes)
			vehicle_scope.Update();
			
		// there was an issue where stale buildings would remain and cause script errors, and due to the unassigned hack, cause a server crash
		// this is a failsafe to handle this situation, see func_vehicle's BlockedBuilding function
		foreach (data in FuncVehicleBuildingDataRemoveQueue)
			FuncVehicleBuildingDataRemoveQueue.remove(data);
		FuncVehicleBuildingDataRemoveQueue.clear();
		
		return -1;	
	}
	
	AddThinkToEnt(FuncVehicleProxy, "FuncVehicleProxy_Think")
}
