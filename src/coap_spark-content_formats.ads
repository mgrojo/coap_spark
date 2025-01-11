with Interfaces;

package CoAP_SPARK.Content_Formats
   with SPARK_Mode => On
is

   -- Generated from Constrained RESTful Environments (CoRE) Parameters
   -- https://www.iana.org/assignments/core-parameters/core-parameters.xhtml
   -- From the registry with id="content-formats"

   package text is

      -- text/plain; charset=utf-8
      plain_charset_utf_8 : constant := 0;

      -- text/css
      css : constant := 20000;

   end text;

   package application is

      -- application/cose; cose-type="cose-encrypt0"
      cose_encrypt0 : constant := 16;

      -- application/cose; cose-type="cose-mac0"
      cose_mac0 : constant := 17;

      -- application/cose; cose-type="cose-sign1"
      cose_sign1 : constant := 18;

      -- application/ace+cbor
      ace_Plus_cbor : constant := 19;

      -- application/link-format
      link_format : constant := 40;

      -- application/xml
      xml : constant := 41;

      -- application/octet-stream
      octet_stream : constant := 42;

      -- application/exi
      exi : constant := 47;

      -- application/json
      json : constant := 50;

      -- application/json-patch+json
      json_patch_Plus_json : constant := 51;

      -- application/merge-patch+json
      merge_patch_Plus_json : constant := 52;

      -- application/cbor
      cbor : constant := 60;

      -- application/cwt
      cwt : constant := 61;

      -- application/multipart-core
      multipart_core : constant := 62;

      -- application/cbor-seq
      cbor_seq : constant := 63;

      -- application/edhoc+cbor-seq
      edhoc_Plus_cbor_seq : constant := 64;

      -- application/cid-edhoc+cbor-seq
      cid_edhoc_Plus_cbor_seq : constant := 65;

      -- application/cose; cose-type="cose-encrypt"
      cose_encrypt : constant := 96;

      -- application/cose; cose-type="cose-mac"
      cose_mac : constant := 97;

      -- application/cose; cose-type="cose-sign"
      cose_sign : constant := 98;

      -- application/cose-key
      cose_key : constant := 101;

      -- application/cose-key-set
      cose_key_set : constant := 102;

      -- application/senml+json
      senml_Plus_json : constant := 110;

      -- application/sensml+json
      sensml_Plus_json : constant := 111;

      -- application/senml+cbor
      senml_Plus_cbor : constant := 112;

      -- application/sensml+cbor
      sensml_Plus_cbor : constant := 113;

      -- application/senml-exi
      senml_exi : constant := 114;

      -- application/sensml-exi
      sensml_exi : constant := 115;

      -- application/yang-data+cbor; id=sid
      yang_data_Plus_cbor_id_sid : constant := 140;

      -- application/coap-group+json
      coap_group_Plus_json : constant := 256;

      -- application/concise-problem-details+cbor
      concise_problem_details_Plus_cbor : constant := 257;

      -- application/swid+cbor
      swid_Plus_cbor : constant := 258;

      -- application/pkixcmp
      pkixcmp : constant := 259;

      -- application/yang-sid+json
      yang_sid_Plus_json : constant := 260;

      -- application/ace-groupcomm+cbor
      ace_groupcomm_Plus_cbor : constant := 261;

      -- application/dots+cbor
      dots_Plus_cbor : constant := 271;

      -- application/missing-blocks+cbor-seq
      missing_blocks_Plus_cbor_seq : constant := 272;

      -- application/pkcs7-mime; smime-type=server-generated-key
      pkcs7_mime_smime_type_server_generated_key : constant := 280;

      -- application/pkcs7-mime; smime-type=certs-only
      pkcs7_mime_smime_type_certs_only : constant := 281;

      -- application/pkcs8
      pkcs8 : constant := 284;

      -- application/csrattrs
      csrattrs : constant := 285;

      -- application/pkcs10
      pkcs10 : constant := 286;

      -- application/pkix-cert
      pkix_cert : constant := 287;

      -- application/aif+cbor
      aif_Plus_cbor : constant := 290;

      -- application/aif+json
      aif_Plus_json : constant := 291;

      -- application/senml+xml
      senml_Plus_xml : constant := 310;

      -- application/sensml+xml
      sensml_Plus_xml : constant := 311;

      -- application/senml-etch+json
      senml_etch_Plus_json : constant := 320;

      -- application/senml-etch+cbor
      senml_etch_Plus_cbor : constant := 322;

      -- application/yang-data+cbor
      yang_data_Plus_cbor : constant := 340;

      -- application/yang-data+cbor; id=name
      yang_data_Plus_cbor_id_name : constant := 341;

      -- application/td+json
      td_Plus_json : constant := 432;

      -- application/tm+json
      tm_Plus_json : constant := 433;

      -- application/voucher+cose
      -- (TEMPORARY - registered 2022-04-12, extension registered 2024-03-01, expires 2025-04-12)
      voucher_Plus_cose : constant := 836;

      -- application/vnd.ocf+cbor
      vnd_Dot_ocf_Plus_cbor : constant := 10000;

      -- application/oscore
      oscore : constant := 10001;

      -- application/javascript
      javascript : constant := 10002;

      -- application/json
      json_deflate : constant := 11050;

      -- application/cbor
      cbor_deflate : constant := 11060;

      -- application/vnd.oma.lwm2m+tlv
      vnd_Dot_oma_Dot_lwm2m_Plus_tlv : constant := 11542;

      -- application/vnd.oma.lwm2m+json
      vnd_Dot_oma_Dot_lwm2m_Plus_json : constant := 11543;

      -- application/vnd.oma.lwm2m+cbor
      vnd_Dot_oma_Dot_lwm2m_Plus_cbor : constant := 11544;

   end application;

   package image is

      -- image/gif
      gif : constant := 21;

      -- image/jpeg
      jpeg : constant := 22;

      -- image/png
      png : constant := 23;

      -- image/svg+xml
      svg_Plus_xml : constant := 30000;

   end image;

   function Is_Text (Content_Type : Interfaces.Unsigned_32) return Boolean
   is (case Content_Type is

          when application.yang_sid_Plus_json => True,
          when application.coap_group_Plus_json => True,
          when application.td_Plus_json => True,
          when application.json_deflate => True,
          when application.senml_Plus_xml => True,
          when image.svg_Plus_xml => True,
          when application.tm_Plus_json => True,
          when application.json_patch_Plus_json => True,
          when application.json => True,
          when text.plain_charset_utf_8 => True,
          when application.senml_etch_Plus_json => True,
          when application.aif_Plus_json => True,
          when application.xml => True,
          when application.sensml_Plus_json => True,
          when application.sensml_Plus_xml => True,
          when application.merge_patch_Plus_json => True,
          when text.css => True,
          when application.vnd_Dot_oma_Dot_lwm2m_Plus_json => True,
          when application.senml_Plus_json => True,
          when others => False);

   function To_String (Content_Type : Interfaces.Unsigned_32) return String
   is (case Content_Type is
         when application.pkixcmp =>
           "application/pkixcmp",
         when application.ace_Plus_cbor =>
           "application/ace+cbor",
         when application.aif_Plus_json =>
           "application/aif+json",
         when application.yang_data_Plus_cbor =>
           "application/yang-data+cbor",
         when application.xml =>
           "application/xml",
         when application.json_patch_Plus_json =>
           "application/json-patch+json",
         when application.javascript =>
           "application/javascript",
         when application.yang_data_Plus_cbor_id_name =>
           "application/yang-data+cbor; id=name",
         when application.voucher_Plus_cose =>
           "application/voucher+cose (TEMPORARY - registered 2022-04-12, " &
           "extension registered 2024-03-01, expires 2025-04-12)",
         when application.vnd_Dot_ocf_Plus_cbor =>
           "application/vnd.ocf+cbor",
         when image.gif =>
           "image/gif",
         when application.merge_patch_Plus_json =>
           "application/merge-patch+json",
         when application.aif_Plus_cbor =>
           "application/aif+cbor",
         when application.pkcs10 =>
           "application/pkcs10",
         when application.csrattrs =>
           "application/csrattrs",
         when application.oscore =>
           "application/oscore",
         when application.edhoc_Plus_cbor_seq =>
           "application/edhoc+cbor-seq",
         when application.octet_stream =>
           "application/octet-stream",
         when application.missing_blocks_Plus_cbor_seq =>
           "application/missing-blocks+cbor-seq",
         when application.tm_Plus_json =>
           "application/tm+json",
         when application.pkix_cert =>
           "application/pkix-cert",
         when application.sensml_exi =>
           "application/sensml-exi",
         when application.concise_problem_details_Plus_cbor =>
           "application/concise-problem-details+cbor",
         when application.senml_Plus_xml =>
           "application/senml+xml",
         when application.link_format =>
           "application/link-format",
         when application.sensml_Plus_xml =>
           "application/sensml+xml",
         when application.ace_groupcomm_Plus_cbor =>
           "application/ace-groupcomm+cbor",
         when application.cose_sign =>
           "application/cose; cose-type=""cose-sign""",
         when application.pkcs8 =>
           "application/pkcs8",
         when application.senml_Plus_json =>
           "application/senml+json",
         when text.css =>
           "text/css",
         when image.jpeg =>
           "image/jpeg",
         when application.cbor_deflate =>
           "application/cbor",
         when application.cose_key =>
           "application/cose-key",
         when application.senml_Plus_cbor =>
           "application/senml+cbor",
         when application.multipart_core =>
           "application/multipart-core",
         when image.svg_Plus_xml =>
           "image/svg+xml",
         when application.coap_group_Plus_json =>
           "application/coap-group+json",
         when application.senml_etch_Plus_json =>
           "application/senml-etch+json",
         when application.pkcs7_mime_smime_type_server_generated_key =>
           "application/pkcs7-mime; smime-type=server-generated-key",
         when application.cose_mac =>
           "application/cose; cose-type=""cose-mac""",
         when application.vnd_Dot_oma_Dot_lwm2m_Plus_json =>
           "application/vnd.oma.lwm2m+json",
         when application.vnd_Dot_oma_Dot_lwm2m_Plus_tlv =>
           "application/vnd.oma.lwm2m+tlv",
         when application.exi =>
           "application/exi",
         when application.cbor_seq =>
           "application/cbor-seq",
         when application.td_Plus_json =>
           "application/td+json",
         when application.json =>
           "application/json",
         when application.json_deflate =>
           "application/json",
         when application.sensml_Plus_json =>
           "application/sensml+json",
         when application.cose_key_set =>
           "application/cose-key-set",
         when application.senml_etch_Plus_cbor =>
           "application/senml-etch+cbor",
         when application.pkcs7_mime_smime_type_certs_only =>
           "application/pkcs7-mime; smime-type=certs-only",
         when application.cose_encrypt0 =>
           "application/cose; cose-type=""cose-encrypt0""",
         when application.cose_sign1 =>
           "application/cose; cose-type=""cose-sign1""",
         when application.vnd_Dot_oma_Dot_lwm2m_Plus_cbor =>
           "application/vnd.oma.lwm2m+cbor",
         when application.cwt =>
           "application/cwt",
         when image.png =>
           "image/png",
         when application.sensml_Plus_cbor =>
           "application/sensml+cbor",
         when application.swid_Plus_cbor =>
           "application/swid+cbor",
         when application.cose_mac0 =>
           "application/cose; cose-type=""cose-mac0""",
         when application.yang_data_Plus_cbor_id_sid =>
           "application/yang-data+cbor; id=sid",
         when text.plain_charset_utf_8 =>
           "text/plain; charset=utf-8",
         when application.cose_encrypt =>
           "application/cose; cose-type=""cose-encrypt""",
         when application.cbor =>
           "application/cbor",
         when application.cid_edhoc_Plus_cbor_seq =>
           "application/cid-edhoc+cbor-seq",
         when application.dots_Plus_cbor =>
           "application/dots+cbor",
         when application.senml_exi =>
           "application/senml-exi",
         when application.yang_sid_Plus_json =>
           "application/yang-sid+json",
         when others => "application/octet-stream");

end CoAP_SPARK.Content_Formats;
