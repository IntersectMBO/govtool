// Active test wallets

import { StaticWallet } from "../lib/wallet/types";

interface DRepWallets {
  dRep1: StaticWallet;
  dRep2: StaticWallet;
  votedDRep: StaticWallet;
}

interface AdaHolderWallets {
  adaHolder1: StaticWallet;
  adaHolder2: StaticWallet;
}

interface ReferenceWallets {
  dRep1: StaticWallet;
  dRep2: StaticWallet;
}
export const bootstrapWallet = {
  type: "bootstrap",
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

export const dRepWallets: DRepWallets = {
  dRep1: {
    type: "dRep",
    payment: {
      private:
        "4f9e979cb0d2c089e319bf81926219957b227d30c5ec7388c2fd14965bdf02c6",
      public:
        "d06c0c27c1221e83dfe8e31b0f5c01b0d0a3522d6f25711fc02c2b82d4b49ecb",
      pkh: "38be1948e77426eaca9f967becc5455b11d3d40fb06b99dd3a817d5e",
    },
    stake: {
      private:
        "b1993b67216233b1a7e4bfccdc5f6677423e923a723d5982f493bc54adad8308",
      public:
        "cfd1df259612825c55803c6eb8f7ab9189eb9cb16e62f3ed6c238ed1cddfd246",
      pkh: "75c7a5e1120727f24236cfb5981ec30fd50a2684a5aca866a123a136",
    },
    dRepId: "drep1whr6tcgjqunlys3ke76es8krpl2s5f5y5kk2se4pywsnvnq50c2",
    address:
      "addr_test1qqutux2gua6zd6k2n7t8hmx9g4d3r575p7cxhxwa82qh6hn4c7j7zys8yleyydk0kkvpasc0659zdp994j5xdgfr5ymqyne2p2",
  },
  dRep2: {
    type: "dRep",
    payment: {
      private:
        "fdab65ff51d676b549841c3547bbc4d3f692a369d5b7956e66fe93a9468c8475",
      public:
        "4be92e72827caaa2238954136ff36f8006b170206dde71c228c237916bcf2e63",
      pkh: "bb17dbac8d4a3452dbb6d0c664e804deb14a0b95ded5274007189c36",
    },
    stake: {
      private:
        "f0f1afbb745320ac2bbba1c0be816b4156e43c6568c48cc027e2aab4249320fc",
      public:
        "919d9725e9c97c555bafaf7370a49b0ba45a791cefe728908ed2398ceb47e7ab",
      pkh: "41868c2b4e5289022a3a1f6f47f86823bc605c609d2c47a2db58e04a",
    },
    dRepId: "",
    address:
      "addr_test1qza30kav349rg5kmkmgvve8gqn0tzjstjh0d2f6qquvfcdjps6xzknjj3ypz5wsldarls6prh3s9ccya93r69k6cup9qkdtk5x",
  },
  votedDRep: {
    // voted DRep
    type: "dRep",
    payment: {
      private:
        "d64e2f706e416d55fb541081a8852dfcbc7593261cf1526eea357e36772a2e9e",
      public:
        "f1804c8e90e2931d076ce184e16427f18a379b9da5200d91be862af0a9a3173a",
      pkh: "f8e61d5f13ab575771af475ac599ad88c7116339f82d2ea969b0e601",
    },
    stake: {
      private:
        "d24b92d38ad455cd952296ae37fe0d0d0e6458107d65d1b5862a87ccf4da880c",
      public:
        "17382aab5a8c9f97bf2fa666dadef945f55f81505a58b4199b0f578311dcbd85",
      pkh: "d6d84c6a5b05cb8f89d24e9d46926975fa1dc08a58b3c26e96c06df7",
    },
    dRepId: "drep16mvyc6jmqh9clzwjf6w5dynfwhapmsy2tzeuym5kcpklwp9myhm",
    address:
      "addr_test1qruwv82lzw44w4m34ar443ve4kyvwytr88uz6t4fdxcwvqwkmpxx5kc9ew8cn5jwn4rfy6t4lgwupzjck0pxa9kqdhmsunzc7p",
  },
};

export const adaHolderWallets: AdaHolderWallets = {
  adaHolder1: {
    type: "adaHolder",
    payment: {
      private:
        "e21d70800240b9bb1b70c96ef909e96ecb46b83686b9c207e8e45f390daca28a",
      public:
        "ebddceb301df8a494855cb7e803e6ed95e078d815f4857c8b6d1a15efbd7386d",
      pkh: "693e466f25213254e061fdc95f8a5f07bf6ef0de0478adbf89a3308f",
    },
    stake: {
      private:
        "a7bdc3b90734a857bae808a479f88fdf93042ae11eec40e34d672b992ec08bbe",
      public:
        "d1a955aed30de26f800653beb3bc83a7de578d9c69edde45a35b15a5629e65f2",
      pkh: "7c4641296645e557c0a6426e140a09d4ba423d158aba1eae06aba797",
    },
    dRepId: "drep103ryz2txghj40s9xgfhpgzsf6jayy0g432apatsx4wnewwgg3qs",
    address:
      "addr_test1qp5nu3n0y5sny48qv87ujhu2turm7mhsmcz83tdl3x3nprmugeqjjej9u4tupfjzdc2q5zw5hfpr69v2hg02up4t57tstdrzh5",
  },

  adaHolder2: {
    type: "adaHolder",
    payment: {
      private:
        "e64d0e806cadd56771d0ec08c8c693212e27d890331462a5520ccb3a0d6b82ee",
      public:
        "728ce39e7e13d64d84c513d83a280ea03cad27e7fbc30fad4cd9c06ec9725460",
      pkh: "d93170064d82eab9dea2b3141bc88503ec80e93c8691fb6b223fe310",
    },
    stake: {
      private:
        "3eb78339bb813aa01ff7b0aaff39a273c5e684ff1fa9def96914d5342b25be76",
      public:
        "18d08f3178c0b467bd64c4be0509d97e640ac9035ad506e2cadf91b4170527d1",
      pkh: "877c17de5bd978526e288334114fada629f699c4e799394aa45c2aad",
    },
    dRepId: "drep1gxrgc26w22ysy236rah507rgyw7xqhrqn5ky0gkmtrsy5mn7tul",
    address:
      "addr_test1qrvnzuqxfkpw4ww752e3gx7gs5p7eq8f8jrfr7mtygl7xyy80stauk7e0pfxu2yrxsg5ltdx98mfn388nyu54fzu92ksmdwpzc",
  },
};

// Reference data DRep wallets
export const referenceWallets: ReferenceWallets = {
  dRep1: {
    type: "dRep",
    payment: {
      private:
        "2aa400c17606fa58df1307e265a070ede1c15df9d8e59683bcd256aefbe689a9",
      public:
        "de337d365e7cec4f01ec7a9af4c37dd65afca0aaf0fa5c828da6898ca68f1968",
      pkh: "93ab1cf6cececd048265573176355a322b7732299bbd624f655af2f6",
    },
    stake: {
      private:
        "7a5eb1dfc051822039b54a782674c527dff8e72f18b44741daa34c13c0440fd0",
      public:
        "902f3e7c43a26dfcb743d9947fcef3a2c8eb355e6d4a3ca40f299821909644ae",
      pkh: "74984fae4ca1715fa1f8759f9d871015ac87f449a85dea6cf9956da1",
    },
    dRepId: "drep1wjvyltjv59c4lg0cwk0empcszkkg0azf4pw75m8ej4k6zuqfvt5",
    address:
      "addr_test1qzf6k88kem8v6pyzv4tnza34tgezkaej9xdm6cj0v4d09an5np86un9pw906r7r4n7wcwyq44jrlgjdgth4xe7v4dkssf9rvq7",
  },
  dRep2: {
    type: "dRep",
    payment: {
      private:
        "36d51e55069121db833aea2c4564a39a8ef95a5d351b4534bc166eb9789f58ee",
      public:
        "20b2147d6e596e8d60bbee91c63854afda49b172c0a210181aa7caeb27663fa9",
      pkh: "bc032a8614a84f5d9ee772f2788954e9d664b4264226cd36d0c4ddae",
    },
    stake: {
      private:
        "b522fc1a4ca31d1ab3a42f3334fd7eef8faf1e67658da7e45e550a1ff73dfb57",
      public:
        "1e1f37ff6091ce4cb87905c05e813ca106dd5a6a34b985ff6bc1fedc60a736b1",
      pkh: "aa22f3a63400c1d96ad118b5cdd300cd039e83ae1957a35b76488194",
    },
    dRepId: "drep14g308f35qrqaj6k3rz6um5cqe5peaqawr9t6xkmkfzqegq53t5z",
    address:
      "addr_test1qz7qx25xzj5y7hv7uae0y7yf2n5ave95yepzdnfk6rzdmt42yte6vdqqc8vk45gckhxaxqxdqw0g8tse2734kajgsx2qmrj65n",
  },
};
