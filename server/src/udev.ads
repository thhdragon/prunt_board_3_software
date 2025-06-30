with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Udev is

   type Udev is null record;
   type Udev_Enumerate is null record;
   type Udev_List_Entry is null record;
   type Udev_Device is null record;

   function Udev_New return access Udev
   with Import => True, Convention => C, External_Name => "udev_new";

   function Udev_Enumerate_New (The_Udev : access Udev) return access Udev_Enumerate
   with Import => True, Convention => C, External_Name => "udev_enumerate_new";

   function Udev_Enumerate_Add_Match_Subsystem
     (The_Udev_Enumerate : access Udev_Enumerate; Subsystem : chars_ptr) return int
   with Import => True, Convention => C, External_Name => "udev_enumerate_add_match_subsystem";

   function Udev_Enumerate_Scan_Devices (The_Udev_Enumerate : access Udev_Enumerate) return int
   with Import => True, Convention => C, External_Name => "udev_enumerate_scan_devices";

   function Udev_Enumerate_Get_List_Entry (The_Udev_Enumerate : access Udev_Enumerate) return access Udev_List_Entry
   with Import => True, Convention => C, External_Name => "udev_enumerate_get_list_entry";

   function Udev_List_Entry_Get_Next (List_Entry : access Udev_List_Entry) return access Udev_List_Entry
   with Import => True, Convention => C, External_Name => "udev_list_entry_get_next";

   function Udev_List_Entry_Get_Name (List_Entry : access Udev_List_Entry) return chars_ptr
   with Import => True, Convention => C, External_Name => "udev_list_entry_get_name";

   function Udev_Device_New_From_Syspath (The_Udev : access Udev; Syspath : chars_ptr) return access Udev_Device
   with Import => True, Convention => C, External_Name => "udev_device_new_from_syspath";

   function Udev_Device_Get_Parent_With_Subsystem_Devtype
     (The_Udev_Device : access Udev_Device; Subsystem : chars_ptr; Devtype : chars_ptr) return access Udev_Device
   with Import => True, Convention => C, External_Name => "udev_device_get_parent_with_subsystem_devtype";

   function Udev_Device_Get_Sysattr_Value
     (The_Udev_Device : access Udev_Device; Sysattr : chars_ptr) return Interfaces.C.Strings.chars_ptr
   with Import => True, Convention => C, External_Name => "udev_device_get_sysattr_value";

   function Udev_Device_Get_Devnode (The_Udev_Device : access Udev_Device) return Interfaces.C.Strings.chars_ptr
   with Import => True, Convention => C, External_Name => "udev_device_get_devnode";

   function Udev_Device_Unref (The_Udev_Device : access Udev_Device) return access Udev_Device
   with Import => True, Convention => C, External_Name => "udev_device_unref";

   function Udev_Enumerate_Unref (The_Udev_Enumerate : access Udev_Enumerate) return access Udev_Enumerate
   with Import => True, Convention => C, External_Name => "udev_enumerate_unref";

   function Udev_Unref (The_Udev : access Udev) return access Udev
   with Import => True, Convention => C, External_Name => "udev_unref";

end Udev;
