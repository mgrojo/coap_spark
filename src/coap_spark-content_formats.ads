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

      -- application/voucher+cose (TEMPORARY - registered 2022-04-12, extension registered 2024-03-01, expires 2025-04-12)
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

end CoAP_SPARK.Content_Formats;
