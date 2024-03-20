import { Box } from '@mui/material';
import { SpacerProps } from '.';

export const Spacer = ({ x, y }: SpacerProps) => <Box pt={y} pr={x} />;
