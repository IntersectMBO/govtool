export type NetworkInfo = {
    current_epoch: number | string | null;
    current_block: number | string | null;
    network_name: string | null;
};


export type GetNetworkInfoResponse = {
    currentTime : string;
    epochNo: number;
    blockNo: number;
    networkName: string;
};
