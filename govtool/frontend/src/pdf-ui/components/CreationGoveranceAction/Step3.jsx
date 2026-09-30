import { ICONS } from '@/consts/icons';
import { Box, Button as MuiButton } from '@mui/material';
import { Button, Typography } from '@atoms';
import ReactMarkdown from 'react-markdown';
import { openInNewTab } from '../../lib/utils';
import MarkdownTypography from '../../lib/markdownRenderer';
import { StepBox, StepButtons, StepHeading, stepButtonSx } from './StepLayout';

// A label/value row as in GovTool's ReviewCreatedGovernanceAction: a
// neutralGray body2 label over the value.
const ReviewLabel = ({ children }) => (
    <Typography
        variant='body2'
        fontWeight={400}
        sx={{ color: 'neutralGray', mb: 0.5 }}
    >
        {children}
    </Typography>
);

const ReviewValue = ({ children, ...rest }) => (
    <Typography
        variant='body2'
        fontWeight={400}
        sx={{ wordBreak: 'break-word' }}
        {...rest}
    >
        {children}
    </Typography>
);

const InfoSection = ({ question, answer, answerTestId }) => {
    return (
        <Box
            sx={{
                mb: 2,
            }}
        >
            <Typography variant='caption' component='span' gutterBottom>
                {question}
            </Typography>
            <MarkdownTypography content={answer} testId={`${answerTestId}`} />
        </Box>
    );
};

const Step3 = ({
    setStep,
    proposalData,
    governanceActionTypes,
    isSmallScreen,
    handleSaveDraft,
}) => {
    const openLink = (link) => openInNewTab(link);
    const selectedGATypeId = proposalData?.gov_action_type_id;
    const pc = proposalData?.proposal_constitution_content;

    return (
        <StepBox>
            <StepHeading title='Review Your Submission' />
            <Box
                sx={{
                    display: 'flex',
                    flexDirection: 'column',
                    gap: 4,
                    mt: 4.25,
                }}
            >
                <Typography
                    variant='title1'
                    component='h5'
                    sx={{ wordBreak: 'break-word' }}
                    data-testid='title-content'
                >
                    {proposalData?.prop_name}
                </Typography>
                <Box>
                    <ReviewLabel>Goverance Action Type</ReviewLabel>
                    <ReviewValue data-testid='governance-action-type-content'>
                        {
                            governanceActionTypes?.find(
                                (x) =>
                                    +x?.value ===
                                    +proposalData?.gov_action_type_id
                            )?.label
                        }
                    </ReviewValue>
                </Box>
                <Box>
                    <ReviewLabel>Abstrtact</ReviewLabel>
                    <MarkdownTypography
                        content={proposalData?.prop_abstract || ''}
                        testId={`abstract-content`}
                    />
                </Box>
                <Box>
                    <ReviewLabel>Motivation</ReviewLabel>
                    <MarkdownTypography
                        content={proposalData?.prop_motivation || ''}
                        testId={`motivation-content`}
                    />
                </Box>
                <Box>
                    <ReviewLabel>Rationale</ReviewLabel>
                    <MarkdownTypography
                        content={proposalData?.prop_rationale || ''}
                        testId={`rationale-content`}
                    />
                </Box>
                {selectedGATypeId == 2
                    ? proposalData?.proposal_withdrawals?.map(
                          (withdrawal, index) => (
                              <Box
                                  sx={{
                                      display: 'flex',
                                      flexDirection: 'column',
                                      gap: 4,
                                  }}
                              >
                                  <Box>
                                      <ReviewLabel>Receiving address</ReviewLabel>
                                      <ReviewValue
                                          data-testid={`receiving-address-${index}-content`}
                                      >
                                          {withdrawal.prop_receiving_address}
                                      </ReviewValue>
                                  </Box>
                                  <Box>
                                      <ReviewLabel>Amount</ReviewLabel>
                                      <ReviewValue
                                          data-testid={`amount-${index}-content`}
                                      >
                                          ₳ {withdrawal.prop_amount}
                                      </ReviewValue>
                                  </Box>
                              </Box>
                          )
                      )
                    : null}
                {selectedGATypeId == 3 && pc ? (
                    <Box
                        sx={{
                            display: 'flex',
                            flexDirection: 'column',
                            gap: 4,
                        }}
                    >
                        <Box>
                            <ReviewLabel>New constitution URL</ReviewLabel>
                            <ReviewValue data-testid='new-constitution-url-content'>
                                {pc.prop_constitution_url}
                            </ReviewValue>
                        </Box>

                        {pc.prop_have_guardrails_script && (
                            <>
                                <Box>
                                    <ReviewLabel>Guardrails script URL</ReviewLabel>
                                    <ReviewValue data-testid='guardrails-script-url-content'>
                                        {pc.prop_guardrails_script_url}
                                    </ReviewValue>
                                </Box>
                                <Box>
                                    <ReviewLabel>
                                        Guardrails script hash
                                    </ReviewLabel>
                                    <ReviewValue data-testid='guardrails-script-hash-content'>
                                        {pc.prop_guardrails_script_hash}
                                    </ReviewValue>
                                </Box>
                            </>
                        )}
                    </Box>
                ) : null}
                {selectedGATypeId == 6 ? (
                    <>
                        <Box>
                            <ReviewLabel>Previous Gov Action Hash</ReviewLabel>
                            <ReviewValue data-testid='previous-ga-hash-content'>
                                {
                                    proposalData?.proposal_hard_fork_content
                                        ?.previous_ga_hash
                                }
                            </ReviewValue>
                        </Box>
                        <Box>
                            <ReviewLabel>Previous Gov Action ID</ReviewLabel>
                            <ReviewValue data-testid='previous-ga-id-content'>
                                {
                                    proposalData?.proposal_hard_fork_content
                                        ?.previous_ga_id
                                }
                            </ReviewValue>
                        </Box>
                        <Box>
                            <ReviewLabel>Major version</ReviewLabel>
                            <ReviewValue data-testid='major-version-content'>
                                {proposalData?.proposal_hard_fork_content?.major}
                            </ReviewValue>
                        </Box>
                        <Box>
                            <ReviewLabel>Minor version</ReviewLabel>
                            <ReviewValue data-testid='minor-version-content'>
                                {proposalData?.proposal_hard_fork_content?.minor}
                            </ReviewValue>
                        </Box>
                    </>
                ) : null}

                {proposalData?.proposal_links?.length > 0 && (
                    <Box>
                        <ReviewLabel>Supporting links</ReviewLabel>
                        <Box
                            sx={{
                                display: 'flex',
                                flexDirection: 'column',
                                alignItems: 'flex-start',
                                gap: 1,
                            }}
                        >
                            {proposalData?.proposal_links?.map(
                                (link, index) => (
                                    <Box
                                        key={index}
                                        sx={{
                                            display: 'flex',
                                            flexDirection: 'row',
                                            alignItems: 'center',
                                            gap: 0.5,
                                            maxWidth: '100%',
                                            minWidth: 0,
                                            p: 0,
                                            textDecoration: 'none',
                                            textTransform: 'none',
                                        }}
                                        component={MuiButton}
                                        onClick={() =>
                                            openLink(link?.prop_link)
                                        }
                                    >
                                        <img
                                            src={ICONS.link}
                                            alt=''
                                            style={{
                                                width: 16,
                                                height: 16,
                                                flexShrink: 0,
                                            }}
                                        />
                                        <Typography
                                            variant='body2'
                                            component='span'
                                            fontWeight={400}
                                            color='primary'
                                            sx={{
                                                textOverflow: 'ellipsis',
                                                overflow: 'hidden',
                                                whiteSpace: 'nowrap',
                                                maxWidth: isSmallScreen
                                                    ? '100%'
                                                    : '800px',
                                            }}
                                            data-testid={`link-${index}-text-content`}
                                        >
                                            {link?.prop_link_text}
                                        </Typography>
                                    </Box>
                                )
                            )}
                        </Box>
                    </Box>
                )}
            </Box>

            <StepButtons
                start={
                    <Button
                        variant='outlined'
                        size='extraLarge'
                        startIcon={
                            <img src={ICONS.editIcon} alt='' width={20} height={20} />
                        }
                        sx={stepButtonSx}
                        onClick={() => setStep(2)}
                        data-testid='back-to-edit-button'
                    >
                        Back to editing
                    </Button>
                }
                end={
                    <>
                        <Button
                            variant='text'
                            size='extraLarge'
                            sx={stepButtonSx}
                            onClick={() => {
                                handleSaveDraft(true);
                            }}
                            data-testid='save-draft-button'
                        >
                            Save Draft
                        </Button>
                        <Button
                            variant='contained'
                            size='extraLarge'
                            sx={stepButtonSx}
                            onClick={() => handleSaveDraft(false)}
                            data-testid='submit-button'
                        >
                            Submit
                        </Button>
                    </>
                }
            />
        </StepBox>
    );
};

export default Step3;
