'use client';

import { Box } from '@mui/material';
import { useLocation } from 'react-router';
import { UsernameModal } from '../components';
import { useAppContext } from '../context/context';
import { setUrlPortsAllowed } from '../lib/utils';
import {
    ProposedGovernanceActions,
    SingleGovernanceAction,
    IdentificationPage,
    CommentReviewPage,
} from '../pages';
import { setAxiosBaseURL } from '../lib/axiosInstance'; // Import axiosInstance and setAxiosBaseURL
import { ScrollToTop } from '../lib/hooks';

const GlobalWrapper = ({ ...props }) => {
    const { pathname: routerPath } = useLocation();
    const pathname = routerPath ?? props?.pathname;

    const { openUsernameModal, setOpenUsernameModal } = useAppContext();

    const {
        pdfApiUrl: GovToolAssemblyPdfApiUrl,
        allowUrlPorts: GovToolAllowUrlPorts,
    } = props;

    function getProposalID(url) {
        const parts = url.split('/');
        const lastSegment = parts[parts.length - 1];

        if (isNaN(lastSegment) || lastSegment.trim() === '') {
            return null;
        }

        return lastSegment;
    }
    function getReviewHash(url) {
        const parts = url.split('/');
        const lastSegment = parts[parts.length - 1];
        if (lastSegment.trim() === '') {
            return null;
        }
        return lastSegment;
    }

    setAxiosBaseURL(GovToolAssemblyPdfApiUrl);
    setUrlPortsAllowed(GovToolAllowUrlPorts);

    const renderComponentBasedOnPath = (path) => {
        if (GovToolAssemblyPdfApiUrl) {
            // if (
            //     !user &&
            //     GovToolAssemblyWalletAPI?.address &&
            //     showIdentificationPage
            // ) {
            //     return <IdentificationPage handleLogin={handleLogin} />;
            // } else {
            //     if (
            //         GovToolAssemblyWalletAPI?.dRepID &&
            //         (GovToolAssemblyWalletAPI?.voter?.isRegisteredAsDRep ||
            //             GovToolAssemblyWalletAPI?.voter
            //                 ?.isRegisteredAsSoleVoter)
            //     ) {
            //         const jwtData = decodeJWT();

            //         if (jwtData) {
            //             let jwtDrepId = jwtData?.dRepID;
            //             if (!jwtDrepId) {
            //                 return (
            //                     <IdentificationPage
            //                         handleLogin={handleLogin}
            //                         isDRep={
            //                             (GovToolAssemblyWalletAPI?.dRepID &&
            //                                 (GovToolAssemblyWalletAPI?.voter
            //                                     ?.isRegisteredAsDRep ||
            //                                     GovToolAssemblyWalletAPI?.voter
            //                                         ?.isRegisteredAsSoleVoter)) ||
            //                             false
            //                         }
            //                     />
            //                 );
            //             }
            //         } else {
            //             return null;
            //         }
            //     }
            if (path.includes('propose')) {
                return <ProposedGovernanceActions />;
            } else if (
                path.includes('proposal_discussion/proposal_comment_review/') &&
                getReviewHash(path)
            ) {
                return <CommentReviewPage reportHash={getReviewHash(path)} />;
            } else if (
                path.includes('proposal_discussion/') &&
                getProposalID(path)
            ) {
                return <SingleGovernanceAction id={getProposalID(path)} />;
            } else if (path.includes('proposal_discussion')) {
                return <ProposedGovernanceActions />;
            } else {
                return <ProposedGovernanceActions />;
            }
        }
        // } else {
        //     return null;
        // }
    };

    return (
        <Box
            component='section'
            display={'flex'}
            flexDirection={'column'}
            flexGrow={1}
        >
            <ScrollToTop />
            {renderComponentBasedOnPath(pathname)}
            <UsernameModal
                open={openUsernameModal}
                handleClose={
                    () =>
                        setOpenUsernameModal({
                            open: false,
                            callBackFn: () => {},
                        }) // Reset open and callbackFn state
                }
            />
        </Box>
    );
};

export default GlobalWrapper;
