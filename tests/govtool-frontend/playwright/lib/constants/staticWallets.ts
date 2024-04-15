import { StaticWallet } from "@types";

export const faucetWallet: StaticWallet = {
  payment: {
    pkh: "b5187cdefbc5b49ddc17b423c079f0717721a03882a3b265bd4c12e0",
    private: "11abec096ef0ea7edbeeee01a1a3f0e9f24a7225c2ee99687fb328146fe85ba6",
    public: "b6a42d4ccc4d26adaec67e8578bf31f13b1b7e640527356248f2ec547f9de6e4",
  },
  stake: {
    pkh: "80f326af300273d19d5a541d45baa42ebc04265816735b026b5f34a4",
    private: "283fd7625ef596f04f21b50ee14a9f4b49f8b1a6f17773cd2e1e69841a111bc1",
    public: "86b08ee3d86cb72d026197a5a710e248d66f28fcff21b4467b75f876b4e6d050",
  },
  dRepId: "drep1zg6zq3ku422ppvfm835rnvzf9ckxtzmy3ayjwylck6s4q9zr5ve",
  address:
    "addr_test1qz63slx7l0zmf8wuz76z8sre7pchwgdq8zp28vn9h4xp9cyq7vn27vqzw0ge6kj5r4zm4fpwhszzvkqkwddsy66lxjjqxnc9zk",
};

export const dRep01Wallet: StaticWallet = {
  payment: {
    private: "4f9e979cb0d2c089e319bf81926219957b227d30c5ec7388c2fd14965bdf02c6",
    public: "d06c0c27c1221e83dfe8e31b0f5c01b0d0a3522d6f25711fc02c2b82d4b49ecb",
    pkh: "38be1948e77426eaca9f967becc5455b11d3d40fb06b99dd3a817d5e",
  },
  stake: {
    private: "b1993b67216233b1a7e4bfccdc5f6677423e923a723d5982f493bc54adad8308",
    public: "cfd1df259612825c55803c6eb8f7ab9189eb9cb16e62f3ed6c238ed1cddfd246",
    pkh: "75c7a5e1120727f24236cfb5981ec30fd50a2684a5aca866a123a136",
  },
  dRepId: "drep1whr6tcgjqunlys3ke76es8krpl2s5f5y5kk2se4pywsnvnq50c2",
  address:
    "addr_test1qqutux2gua6zd6k2n7t8hmx9g4d3r575p7cxhxwa82qh6hn4c7j7zys8yleyydk0kkvpasc0659zdp994j5xdgfr5ymqyne2p2",
};

export const adaHolder01Wallet: StaticWallet = {
  payment: {
    private: "e21d70800240b9bb1b70c96ef909e96ecb46b83686b9c207e8e45f390daca28a",
    public: "ebddceb301df8a494855cb7e803e6ed95e078d815f4857c8b6d1a15efbd7386d",
    pkh: "693e466f25213254e061fdc95f8a5f07bf6ef0de0478adbf89a3308f",
  },
  stake: {
    private: "a7bdc3b90734a857bae808a479f88fdf93042ae11eec40e34d672b992ec08bbe",
    public: "d1a955aed30de26f800653beb3bc83a7de578d9c69edde45a35b15a5629e65f2",
    pkh: "7c4641296645e557c0a6426e140a09d4ba423d158aba1eae06aba797",
  },
  dRepId: "drep103ryz2txghj40s9xgfhpgzsf6jayy0g432apatsx4wnewwgg3qs",
  address:
    "addr_test1qp5nu3n0y5sny48qv87ujhu2turm7mhsmcz83tdl3x3nprmugeqjjej9u4tupfjzdc2q5zw5hfpr69v2hg02up4t57tstdrzh5",
};

export const adaHolder02Wallet: StaticWallet = {
  payment: {
    private: "e64d0e806cadd56771d0ec08c8c693212e27d890331462a5520ccb3a0d6b82ee",
    public: "728ce39e7e13d64d84c513d83a280ea03cad27e7fbc30fad4cd9c06ec9725460",
    pkh: "d93170064d82eab9dea2b3141bc88503ec80e93c8691fb6b223fe310",
  },
  stake: {
    private: "3eb78339bb813aa01ff7b0aaff39a273c5e684ff1fa9def96914d5342b25be76",
    public: "18d08f3178c0b467bd64c4be0509d97e640ac9035ad506e2cadf91b4170527d1",
    pkh: "877c17de5bd978526e288334114fada629f699c4e799394aa45c2aad",
  },
  dRepId: "drep1gxrgc26w22ysy236rah507rgyw7xqhrqn5ky0gkmtrsy5mn7tul",
  address:
    "addr_test1qrvnzuqxfkpw4ww752e3gx7gs5p7eq8f8jrfr7mtygl7xyy80stauk7e0pfxu2yrxsg5ltdx98mfn388nyu54fzu92ksmdwpzc",
};

// Does not takes part in transaction
export const user01Wallet: StaticWallet = {
  payment: {
    private: "5dc9157e04158db026c9f1adc05292f93e9cf068e23b15bfd23c6ce02b73ffbf",
    public: "5ee0bbfb4f2befa3c5574b59a9c5b95d211423643ef35d21b49fa05a2d1b26f0",
    pkh: "9fc9b8e0f4e1a75e7fcef6375d55abe5ed7ce6df259c20d8f93194a7",
  },
  stake: {
    private: "b8da5e0315dfe7ae07bbbb686691d8f9a9f12ad3a38826e2b6a30fffacdad27f",
    public: "6963ddeb19d59eef0bc7aa79be5b39c556e027dc56e1cbd452f702ae50f06d98",
    pkh: "19bb4459bcf4dbaa240b8370a77c8d4525b01c2877b053d43e2c9b22",
  },
  address:
    "addr1qx0unw8q7ns6whnlemmrwh2440j76l8xmujecgxclyceffcehdz9n085mw4zgzurwznher29ykcpc2rhkpfag03vnv3qvdk48p",
  dRepId: "drep1rxa5gkdu7nd65fqtsdc2wlydg5jmq8pgw7c984p79jdjyyqmyqs",
};

export const adaHolderWallets = [adaHolder01Wallet, adaHolder02Wallet];

export const userWallets = [user01Wallet];

export const dRepWallets = [dRep01Wallet];
