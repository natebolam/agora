{ lib, ... }:

let
  inherit (lib) mkOption types;

  lootLogOptions = {
    backends = mkOption {
      # TODO: support other backends
      type = types.listOf (types.enum ["stderr"]);
      default = ["stderr"];
      apply = map (x: { type = x; });
    };
    min-severity = mkOption {
      type = types.enum ["Debug" "Info" "Warning" "Error"];
      default = "Warning";
    };
  };
in

{
  options.agora = {
    api = {
      listen_addr = mkOption {
        type = types.str;
        default = "*:8190";
      };
      serve_docs = mkOption {
        type = types.bool;
        default = true;
      };
    };

    logging = lootLogOptions;

    contract = {
      address = mkOption {
        type = types.str;
        example = "KT1someContractAddressForExampleHere";
      };
      contract_block_level = mkOption {
        type = types.ints.positive;
        example = 176671;
      };
    };

    node_addr = mkOption {
      type = types.str;
      example = "tezos.example.com:8732";
    };

    db = {
      conn_string = mkOption {
        type = types.str;
        default = "host=/run/postgresql dbname=agora";
      };
      max_connections = mkOption {
        type = types.ints.positive;
        default = 200;
      };
    };

    discourse = {
      host = mkOption {
        type = types.str;
        example = "https://discourse.example.com";
      };
      category = mkOption {
        type = types.str;
        default = "Proposals";
      };
      api_username = mkOption {
        type = types.str;
        default = "agora";
      };
      api_key = mkOption {
        type = types.str;
        example = "d06ca53322d1fbaf383a6394d6c229e56871342d2cad953a0fe26c19df7645ba";
      };
    };
  };
}
