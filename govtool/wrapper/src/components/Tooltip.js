import { styled } from "@mui/material";
import * as TooltipMUI from "@mui/material/Tooltip";
import Typography from "@mui/material/Typography";

const StyledTooltip = styled(({ className, ...props }) => (
  // eslint-disable-next-line react/jsx-pascal-case
  <TooltipMUI.default {...props} arrow classes={{ popper: className }} />
))(() => ({
  [`& .${TooltipMUI.tooltipClasses.arrow}`]: {
    color: "rgb(36, 34, 50)",
  },
  [`& .${TooltipMUI.tooltipClasses.tooltip}`]: {
    backgroundColor: "rgb(36, 34, 50)",
    padding: 12,
  },
}));

export const Tooltip = ({ heading, paragraphOne, paragraphTwo, ...rest }) => (
  <StyledTooltip
    {...rest}
    enterTouchDelay={0}
    leaveTouchDelay={1000}
    title={
      <>
        {heading && (
          <Typography fontSize={16} fontWeight={400} color="#FBFBFF">
            {heading}
          </Typography>
        )}
        <Typography
          mt={0.5}
          fontSize={14}
          fontWeight={400}
          color="rgb(170, 170, 170)"
        >
          {paragraphOne && paragraphOne}
          {paragraphTwo && (
            <>
              <br /> <br />
              {paragraphTwo}
            </>
          )}
        </Typography>
      </>
    }
  />
);
