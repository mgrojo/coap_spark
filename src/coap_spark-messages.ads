with CoAP_SPARK.Log;
with CoAP_SPARK.Options.Lists;
with Interfaces;
with RFLX.CoAP;
with RFLX.RFLX_Types;

package CoAP_SPARK.Messages
   with SPARK_Mode
is

   use type RFLX.RFLX_Types.Bytes_Ptr;

   subtype Response_Code is RFLX.CoAP.Code_Class
                              range RFLX.CoAP.Success .. RFLX.CoAP.Server_Error;

   type Response_Kind (Code_Class : Response_Code := RFLX.CoAP.Success) is
      record
         case Code_Class is
            when RFLX.CoAP.Success =>
               null;
            when RFLX.CoAP.Client_Error =>
               Client_Error_Code : RFLX.CoAP.Client_Error_Response;
            when RFLX.CoAP.Server_Error =>
               Server_Error_Code : RFLX.CoAP.Server_Error_Response;
         end case;
      end record;

   subtype Payload_Ptr is RFLX.RFLX_Types.Bytes_Ptr
   with
     Dynamic_Predicate =>
       Payload_Ptr = null
       or else Payload_Ptr'Length <= CoAP_SPARK.Max_Payload_Length;

   type Content is record
      Options : CoAP_SPARK.Options.Lists.Vector
                          (CoAP_SPARK.Max_Number_Of_Options);
      Format : Interfaces.Unsigned_32 := 0;
      Payload : Payload_Ptr;
   end record;

   --  Log the content of a message according to the log levels for options and
   --  payload.
   procedure Print_Content
     (Item              : Content;
      General_Log_Level : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Debug;
      Log_Level_Payload : CoAP_SPARK.Log.Level_Type := CoAP_SPARK.Log.Info)
      with Pre => CoAP_SPARK.Log."<=" (General_Log_Level, Log_Level_Payload);

   -- Return the image of a response kind. When Long is True, the image includes
   -- the human readable description of the response code, otherwise it only
   -- includes the code in standard format.
   function Image (Item : Response_Kind; Long : Boolean := True) return String
   with
     Post => Image'Result'First = 1;

   procedure Finalize (Item : in out Content)
   with
     Post =>
       CoAP_SPARK.Options.Lists.Is_Empty (Item.Options)
       and then Item.Payload = null;

end CoAP_SPARK.Messages;
