import { Box, InputBase } from "@mui/material";
import { FC } from "react";
import Search from "@mui/icons-material/Search";
import { theme } from "@/theme";
import { IMAGES } from "@/consts";

interface SyncAiSearchProps {
  searchText: string;
  setSearchText: (text: string) => void;
}
export const SyncAiSearch: FC<SyncAiSearchProps> = ({
  searchText,
  setSearchText,
}) => {
  const {
    palette: { boxShadow2 },
  } = theme;

  return (
    <Box alignItems="center" display="flex" justifyContent="row-start">
      {/* InputBase values copied from DataActionsBar */}
      <InputBase
        inputProps={{ "data-testid": "search-input" }}
        onChange={(e) => setSearchText(e.target.value)}
        placeholder="Search..."
        value={searchText}
        startAdornment={
          <Search
            style={{
              color: "#99ADDE",
              height: 16,
              marginRight: 4,
              width: 16,
            }}
          />
        }
        sx={{
          bgcolor: "white",
          border: 1,
          borderColor: "secondaryBlue",
          borderRadius: 50,
          boxShadow: `2px 2px 20px 0px ${boxShadow2}`,
          fontSize: 11,
          fontWeight: 500,
          height: 48,
          padding: "16px 24px",
          maxWidth: 500,
        }}
      />
      <img
        alt="sync-ai-logo"
        height={35}
        src={IMAGES.syncAiLogo}
        style={{ objectFit: "contain" }}
      />
    </Box>
  );
};
