--  Configuration for coap_spark generated by Alire
pragma Restrictions (No_Elaboration_Code);
pragma Style_Checks (Off);

package Coap_Spark_Config is
   pragma Pure;

   Crate_Version : constant String := "0.2.0-dev";
   Crate_Name : constant String := "coap_spark";

   Alire_Host_OS : constant String := "linux";

   Alire_Host_Arch : constant String := "x86_64";

   Alire_Host_Distro : constant String := "ubuntu";

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := development;

end Coap_Spark_Config;
