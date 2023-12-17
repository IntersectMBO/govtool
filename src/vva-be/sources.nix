{
  pkgs = import
    (builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "c9ece0059f42e0ab53ac870104ca4049df41b133";
    })
    { };
}
