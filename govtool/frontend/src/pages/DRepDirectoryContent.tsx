import { Box } from "@mui/material";
import { FC } from "react";
import { AutomatedVotingOptions, DRepCard } from "@organisms";
import { Typography } from "@atoms";
import { Trans, useTranslation } from "react-i18next";

interface DRepDirectoryContentProps {
  isConnected?: boolean;
}

export const DRepDirectoryContent: FC<DRepDirectoryContentProps> = ({
  isConnected,
}) => {
  const { t } = useTranslation();

  const ada = 1234567;

  return (
    <Box display="flex" flexDirection="column" gap={4}>
      {isConnected && (
        <div>
          <Typography variant="title2" sx={{ mb: 2 }}>
            <Trans i18nKey="dRepDirectory.myDelegation" values={{ ada }} />
          </Typography>
          <DRepCard key={data[0]} isConnected={isConnected} {...data[0]} />
        </div>
      )}

      {isConnected && (
        <div>
          <Typography variant="title2" sx={{ mb: 2 }}>{t("dRepDirectory.delegationOptions")}</Typography>
          <AutomatedVotingOptions />
        </div>
      )}

      <div>
        <Typography fontSize={18} fontWeight={500}>{t("dRepDirectory.listTitle")}</Typography>
        <Box component="ul" p={0} display="flex" flexDirection="column" gap={3}>
          {data.map((dRep) => (
            <Box key={dRep.id} component="li" sx={{ listStyle: "none" }}>
              <DRepCard key={dRep.id} isConnected={isConnected} {...dRep} />
            </Box>
          ))}
        </Box>
      </div>
    </Box>
  );
};

const data = [
  {
    name: "DRep 1",
    id: "1kejngelrngekngeriogj3io4j3gnd3",
    votingPower: 3000000,
    status: "active",
  },
  {
    name: "DRep 2",
    id: "1kejrngelrngekngeriogfrej3io4fj3gn3",
    votingPower: 10000000,
    status: "active",
  },
  {
    name: "DRep 1",
    id: "1kejngelrngekngeriogj3io4j3gnd3",
    votingPower: 1000000,
    status: "active",
  },
  {
    name: "DRep 2",
    id: "1kejrngelrngekngeriogfrej3io4fj3gn3",
    votingPower: 9900000000000000,
    status: "active",
  },
  {
    name: "DRep 1",
    id: "1kejngelrngekngeriogj3io4j3gn3",
    votingPower: 12345678,
    status: "active",
  },
  {
    name: "DRep 2",
    id: "1kejrngelrngekngeriogfrej3io4j3gn3",
    votingPower: 1234567800,
    status: "active",
  },
  {
    name: "DRep 4",
    id: "1kejrngelkngeriogj3io4j3gn3",
    votingPower: 12345678000000,
    status: "retired",
  },
  {
    name: "DRep 3",
    id: "1kejrngelrngekngeriogj3io4j3gn2",
    votingPower: 123456,
    status: "active",
  },
  {
    name: "Some dreps can have a very long name and it will be displayed correctly",
    id: "1kejrngelrngekngeriogj3io4j3gn3",
    votingPower: 123456,
    status: "inactive",
  },
];
