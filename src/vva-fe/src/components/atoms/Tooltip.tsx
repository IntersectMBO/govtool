import { styled } from "@mui/material";
import * as TooltipMUI from "@mui/material/Tooltip";
import Typography from "@mui/material/Typography";

type TooltipProps = Omit<TooltipMUI.TooltipProps, "title"> & {
  heading?: string;
  paragraphOne?: string;
  paragraphTwo?: string;
};

export const Tooltip = ({
  heading,
  paragraphOne,
  paragraphTwo,
  ...tooltipProps
}: TooltipProps) => {
  return (
    <StyledTooltip
      {...tooltipProps}
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
};

const StyledTooltip = styled(
  ({ className, ...props }: TooltipMUI.TooltipProps) => (
    <TooltipMUI.default {...props} arrow classes={{ popper: className }} />
  )
)(() => ({
  [`& .${TooltipMUI.tooltipClasses.arrow}`]: {
    color: "rgb(36, 34, 50)",
  },
  [`& .${TooltipMUI.tooltipClasses.tooltip}`]: {
    backgroundColor: "rgb(36, 34, 50)",
    padding: 12,
  },
}));
