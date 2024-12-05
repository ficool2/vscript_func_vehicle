# vscript_func_vehicle
Accurate recreation of [func_vehicle](https://developer.valvesoftware.com/wiki/Func_vehicle) with VScript in TF2.

If you are looking for HL2-style vehicles, see [vscript_vehicle](https://github.com/ficool2/vscript_vehicle) instead.

# Usage
Create your vehicle out of brushes and make it into a `func_tracktrain` entity.

The vehicle must be built facing the -X direction. 
To change the facing angle when it spawns, set the Bank Angle property (e.g. 180 will rotate it 180 degrees).

Next, create a script for your vehicle. To do this, go into `scripts/vscripts/func_vehicles/` and copy the `jeep.nut` vehicle script, and rename it to something else

Modify the properties inside the script as desired. 
Note that you need to define the dimensions for collision, you can do this by drawing a box over your vehicle and noting down the dimensions.

Assign the script to the `func_tracktrain` as `func_vehicles/NAME` under Entity Scripts (replace `NAME` with whatever your script is called).

Finally, you will need to create a trigger_multiple that defines the area where you can control the vehicle. 
Parent it to the vehicle, and assign it the `func_vehicle_controls` Entity Script.
You can also add filters to this trigger to exclude teams or certain people from controlling vehicles.

Optionally, attach `info_observer_point` entities to your vehicles so spectators get a cool view.

And that's all! The vehicle should now be ready to go and drive. 

Vehicles are driven by being inside the trigger and using the Action key (default: H) or +use.

**NOTE**: the vehicles have custom sounds. Make sure to pack this into the BSP when uploading the map!

## Example
There is a example VMF in the `mapsrc` folder that contains two jeep vehicles ready to go.

# Limitations
The vehicle collision is very primitive (remember: `func_vehicle` was made with year 2000 standards!), so don't be surprised if the vehicles get stuck, clip into geometry etc.

Vehicles should have enough space to contain players. Keep in mind the player size is inflated when rotated too. The code will attempt to keep players attached as much as possible.