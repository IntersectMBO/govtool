import {
    loginUser,
    getLoggedInUserInfo,
    getChallenge,
    getRefreshToken,
} from '../lib/api';
import {
    saveDataInSession,
    getDataFromSession,
    utf8ToHex,
    clearSession,
    decodeJWT,
} from '../lib/utils';

export const loginUserToApp = async ({
    wallet,
    setUser,
    setOpenUsernameModal,
    callBackFn,
    trigerSignData = true,
    clearStates,
    setPDFUsername,
    isDRep = false,
    addErrorAlert,
    addSuccessAlert,
    addChangesSavedAlert,
    isStale = () => false,
}) => {
    // Without a usable wallet there is nothing to sign with or validate
    // against; the session is kept until the wallet is ready.
    if (!wallet) return;
    // Never ask for a challenge without a key to sign it with.
    if (
        !isDRep &&
        trigerSignData &&
        !getDataFromSession('pdfUserJwt') &&
        !wallet?.stakeKey
    ) {
        return;
    }
    if (isDRep && !wallet?.dRepID) return;

    try {
        if (!isDRep) {
            if (getDataFromSession('pdfUserJwt')) {
                const jwt = decodeJWT(); // Get JWT from session

                if (jwt?.stakeKey) {
                    // Check wallet stake key and jwt stake key
                    if (wallet) {
                        if (jwt.stakeKey !== wallet?.stakeKey) {
                            clearStates();
                            clearSession();
                            return;
                        }
                    } else {
                        clearStates();
                        clearSession();
                        return;
                    }
                } else {
                    clearStates();
                    clearSession();
                    return;
                }

                if (jwt?.dRepID) {
                    // Check wallet stake key and jwt stake key
                    if (wallet) {
                        if (wallet.dRepID) {
                            if (jwt.dRepID !== wallet?.dRepID) {
                                clearStates();
                                clearSession();
                                return;
                            }
                        } else {
                            clearStates();
                            clearSession();
                            return;
                        }
                    } else {
                        clearStates();
                        clearSession();
                        return;
                    }
                }

                let loggedInUser = await getLoggedInUserInfo();
                if (isStale()) return;
                if (!loggedInUser) {
                    // A 401 or an expired JWT: refresh once, else sign out.
                    loggedInUser = await refreshAndGetUser(wallet, isStale);
                    if (isStale()) return;
                    if (!loggedInUser) {
                        clearStates();
                        clearSession();
                        return;
                    }
                }
                setUser({
                    user: {
                        ...loggedInUser,
                    },
                });
                !callBackFn && addSuccessAlert('Successfully logged in.');

                if (loggedInUser && !loggedInUser?.govtool_username) {
                    setOpenUsernameModal({
                        open: true,
                        callBackFn: callBackFn
                            ? () => callBackFn(loggedInUser)
                            : () => {},
                    });
                } else {
                    if (setPDFUsername) {
                        setPDFUsername(loggedInUser?.govtool_username);
                    }
                    if (callBackFn) {
                        callBackFn(loggedInUser);
                    }
                }
            } else {
                if (trigerSignData) {
                    const keyToSign = wallet?.stakeKey;

                    const challengeRes = await getChallenge({
                        query: `?identifier=${keyToSign}`,
                    });

                    const { message } = challengeRes;
                    const messageHex = utf8ToHex(message);

                    const signedMessage = await wallet?.cip95.signData(
                        keyToSign,
                        messageHex
                    );

                    const userResponse = await loginUser({
                        identifier: keyToSign,
                        signedMessage: {
                            ...signedMessage,
                            expectedSignedMessage: message,
                        },
                    });

                    if (!userResponse) return;
                    saveDataInSession('pdfUserJwt', userResponse?.jwt);
                    setUser(userResponse);
                    addSuccessAlert('Successfully signed data with stake key.');

                    if (userResponse && !userResponse?.user?.govtool_username) {
                        setOpenUsernameModal({
                            open: true,
                            callBackFn: callBackFn
                                ? () => callBackFn(userResponse?.user)
                                : () => {},
                        });
                    } else {
                        if (setPDFUsername) {
                            setPDFUsername(
                                userResponse?.user?.govtool_username
                            );
                        }

                        if (callBackFn) {
                            callBackFn(userResponse?.user);
                        }
                    }
                }
            }
        } else {
            if (trigerSignData) {
                const keyToSign = wallet?.dRepID;
                const challengeRes = await getChallenge({
                    query: `?identifier=${keyToSign}`,
                });
                const { message } = challengeRes;
                const messageHex = utf8ToHex(message);

                const signedMessage = await wallet?.cip95.signData(
                    keyToSign,
                    messageHex
                );

                const userResponse = await loginUser({
                    identifier: keyToSign,
                    signedMessage: {
                        ...signedMessage,
                        expectedSignedMessage: message,
                    },
                });

                if (!userResponse) return;
                saveDataInSession('pdfUserJwt', userResponse?.jwt);
                setUser(userResponse);
                addSuccessAlert('Successfully signed data with dRep key.');

                if (userResponse && !userResponse?.user?.govtool_username) {
                    setOpenUsernameModal({
                        open: true,
                        callBackFn: callBackFn
                            ? () => callBackFn(userResponse?.user)
                            : () => {},
                    });
                } else {
                    if (setPDFUsername) {
                        setPDFUsername(userResponse?.user?.govtool_username);
                    }

                    if (callBackFn) {
                        callBackFn(userResponse?.user);
                    }
                }
            }
        }
    } catch (error) {
        console.error(error);
        let errorMessage = 'Something went wrong';
        if (error?.response?.data?.error?.message) {
            errorMessage = error?.response?.data?.error?.message;
        }
        addErrorAlert(errorMessage);
        if (error?.code !== 3) {
            clearStates();
            clearSession();
        }
    }
};
const refreshAndGetUser = async (wallet, isStale) => {
    try {
        const refreshed = await getRefreshToken();
        if (isStale() || !refreshed?.jwt) return undefined;
        saveDataInSession('pdfUserJwt', refreshed.jwt);
        // The refresh cookie is shared by every tab; only keep a JWT that
        // belongs to this wallet.
        if (decodeJWT()?.stakeKey !== wallet?.stakeKey) return undefined;
        return await getLoggedInUserInfo();
    } catch (error) {
        console.error('Error refreshing token:', error);
        return undefined;
    }
};

export const cleanObject = (obj) => {
    if (Array.isArray(obj)) {
        return obj.map(cleanObject);
    }
    if (obj !== null && typeof obj === 'object') {
        const newObj = {};
        for (const key in obj) {
            if (!obj.hasOwnProperty(key)) continue;
            if (['id', 'createdAt', 'updatedAt'].includes(key)) {
                continue;
            }
            let value = cleanObject(obj[key]);
            if (['data', 'attributes'].includes(key)) {
                if (
                    value &&
                    typeof value === 'object' &&
                    !Array.isArray(value)
                ) {
                    for (const nestedKey in value) {
                        if (value.hasOwnProperty(nestedKey)) {
                            newObj[nestedKey] = value[nestedKey];
                        }
                    }
                } else {
                    newObj[key] = value;
                }
            } else {
                newObj[key] = value;
            }
        }
        return newObj;
    }
    return obj;
};

// A comment without reports, such as one from the 2025 budget proposals
// archive (which holds none), is not restricted.
export const isCommentRestricted = (curComment) => {
    const reports = curComment?.attributes?.comments_reports?.data ?? [];
    let banned = reports.some((report) => {
        return report.attributes.moderation_status === true;
    });
    let x = reports.filter((report) => {
        return report.attributes.moderation_status !== false;
    });
    if (banned || x.length >= 3) {
        return true;
    }
    return false;
};

export const checkShowValidation = (drepRequired = false, walletAPI, user) => {
    let showButton = false;

    if (!walletAPI) {
        showButton = true;
    } else if (!user) {
        showButton = true;
    } else if (!user?.user?.govtool_username) {
        showButton = true;
    } else if (drepRequired) {
        if (checkIfDrepIsSignedIn(walletAPI)) {
            showButton = true;
        } else {
            showButton = false;
        }
    } else {
        showButton = false;
    }

    return showButton;
};

export const checkIfDrepIsSignedIn = (walletAPI) => {
    const jwtData = decodeJWT();
    const isDrep =
        walletAPI?.voter?.isRegisteredAsDRep ||
        walletAPI?.voter?.isRegisteredAsSoleVoter;
    const hasDrepID = !!jwtData?.dRepID;

    if (isDrep && !hasDrepID) {
        return true;
    }

    return false;
};
