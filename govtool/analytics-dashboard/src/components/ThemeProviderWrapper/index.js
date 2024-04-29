'use client';
import { Sidebar } from '@/components';
import Loader from '@/components/Loader';
import { useAppContext } from '@/context/context';
import { usePathname } from '@/navigation';
import { Box } from '@mui/material';
import {
    ThemeProvider,
    createTheme,
    responsiveFontSizes,
} from '@mui/material/styles';
import { useEffect, useRef, useState } from 'react';

let theme = createTheme({
    components: {
        MuiButton: {
            styleOverrides: {
                root: {
                    fontFamily: 'Poppins',
                    padding: '12px 24px',
                    borderRadius: 100,
                    textTransform: 'none',
                },
            },
        },
        MuiCard: {
            styleOverrides: {
                root: { borderRadius: 16 },
            },
        },
        MuiCardContent: {
            styleOverrides: {
                root: { padding: '20px 24px' },
            },
        },
        MuiTypography: {
            styleOverrides: {
                root: { fontFamily: 'Poppins' },
                h4: { fontWeight: 500, color: '#212A3D' },
                body1: { color: '#506288' },
                subtitle2: { color: '#506288' },
                caption: { color: '#212A3D', fontWeight: 400 },
            },
            defaultProps: {
                variantMapping: {
                    h4: 'h1',
                },
            },
        },
        MuiInputBase: {
            styleOverrides: {
                input: {
                    fontFamily: 'Poppins',
                },
                root: {
                    fontFamily: 'Poppins',
                }
            },
        },
        MuiOutlinedInput: {
            styleOverrides: {
                input: {
                    fontFamily: 'Poppins',
                },
                root: {
                    fontFamily: 'Poppins',
                }
            },
        },
    },
    palette: {
        primary: {
            main: '#3052F5',
        },
        secondary: {
            main: '#D63F1E',
        },
        text: {
            main: '#fff',
            gray: '#506288',
            black: '#212A3D',
            primaryBlue: "#0033AD"
        },
        background: {
            default: '#F2F4F8',
            primaryContainer: '#0114BC',
            info: '#EDEBFF',
            white: '#fff',
        },
        success: {
            main: '#CEF3D4',
        },
        icon: {
            disabled: '#BDBDBD'
        }
    },
});
theme.typography = {
    ...theme.typography,
    fontFamily: 'Poppins, sans-serif',
};
theme = responsiveFontSizes(theme);

function ThemeProviderWrapper({ children }) {
    const { pageBackground } = useAppContext();
    const [windowWidth, setWindowWidth] = useState(0);

    const drawerWidth = 340;
    const pathname = usePathname();

    useEffect(() => {
        if (window) {
            setWindowWidth(window?.innerWidth);
        }
        const handleResize = () => {
            setWindowWidth(window?.innerWidth);
        };

        window?.addEventListener('resize', handleResize);

        return () => window?.removeEventListener('resize', handleResize);
    }, []);



    return (
        <ThemeProvider theme={theme}>
            {/* <Loader /> */}
            {pathname.includes('/dashboard') && (
                <Sidebar drawerWidth={drawerWidth} />
            )}

            <Box
                component="main"
                sx={{
                    flexGrow: 1,
                    p: 0,
                    backgroundColor: theme.palette.background.default,
                    backgroundImage: `url('/assets/svgs/bg-image.svg')`,
                    backgroundSize: 'cover', // This ensures the background covers the available space
                    backgroundPosition: 'center', // Centers the background image
                    backgroundRepeat: 'no-repeat', // Prevents the background image from repeating
                    height: '100vh', // Makes sure the Box fills the viewport height
                    overflow: 'auto',
                }}
            >
                {children}
            </Box>
        </ThemeProvider>
    );
}

export default ThemeProviderWrapper;