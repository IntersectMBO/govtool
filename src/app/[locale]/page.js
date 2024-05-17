'use client';

import { Grid, Card, CardContent, Typography, Box } from '@mui/material';
import { PeopleAltOutlined, ArticleOutlined, AccountBalanceWalletOutlined, HowToVoteOutlined, PersonOutline, BalanceOutlined } from '@mui/icons-material';
import { useTheme } from '@mui/material/styles';
import getGoogleData from '@/lib/api';
import { useEffect, useState } from 'react';
import { Link } from '@/navigation';

function Dashboard() {

	const [stats, setStats] = useState([
		// { title: 'Total users', value: 0, Icon: PeopleAltOutlined },
		{ title: 'Unique users', value: 0, Icon: PeopleAltOutlined },
		{ title: 'Wallet connections', value: 0, Icon: AccountBalanceWalletOutlined },
		{ title: 'Proposal views', value: 0, Icon: ArticleOutlined },
		{ title: 'DRep Registrations', value: 0, Icon: PersonOutline },
		{ title: 'Votes submitted', value: 0, Icon: HowToVoteOutlined },
		{ title: 'Delegations', value: 0, Icon: BalanceOutlined }
	])


	const fetchData = async () => {
		const resp = await getGoogleData()
		const statMap = resp.reduce((acc, event) => {
			acc[event.eventName] = event.eventCount;
			return acc;
		}, {});

		let statsTemplate = [
			// { title: 'Total users', value: statMap['user_engagement'], Icon: PeopleAltOutlined },
			{ title: 'Unique users', value: statMap['first_visit'], Icon: PeopleAltOutlined },
			{ title: 'Wallet connections', value: +statMap['Connect wallet'] + +statMap['Connect your wallet'], Icon: AccountBalanceWalletOutlined },
			{ title: 'Proposal views', value: statMap['View proposal details'], Icon: ArticleOutlined },
			{ title: 'DRep Registrations', value: statMap['Register'], Icon: PersonOutline },
			{ title: 'Votes submitted', value: statMap['Vote'], Icon: HowToVoteOutlined },
			{ title: 'Delegations', value: statMap['Delegate'], Icon: BalanceOutlined },
		];
		setStats(statsTemplate)
	}

	useEffect(() => {
		fetchData()
	}, [])



	const theme = useTheme();

	return (
		<Box display="flex" flexDirection="column" alignItems="center" justifyContent="space-between" height="100%" sx={{ color: (theme) => theme?.palette?.text?.black }}>
			<Box display="flex" flexDirection="column" alignItems="start" gap={5} padding={5}>
				<Box display="flex" flexDirection="column" alignItems="start" gap={1}>
					<Typography variant="h5" component="h1">
						SanchoNet Govtool
					</Typography>
					<Typography variant="h4" component="h2">
						Participation Dashboard
					</Typography>
					<Typography variant="subtitle1" sx={{ color: (theme) => theme?.palette?.text?.gray }}>
						This dashboard shows the overall participation and usage of SanchoNet Govtool from 1st of December 2023
					</Typography>
				</Box>


				<Grid container spacing={4}>
					{stats.map((stat, index) => (
						<Grid item xs={12} sm={12} md={6} key={index}>
							<Card
								sx={{ background: (theme) => theme?.palette?.background?.info, width: '100%' }}
							>
								<CardContent sx={{ display: 'flex', flexDirection: 'column', alignItems: 'center', justifyContent: 'center', height: '100%' }}>
									<stat.Icon style={{ fontSize: 60, color: theme?.palette?.text?.black, paddingBottom: 20 }} />

									<Typography variant="subtitle1" sx={{ color: (theme) => theme?.palette?.text?.gray }}>
										{stat.title}
									</Typography>
									<Typography variant="h4">
										{stat.value}
									</Typography>
								</CardContent>
							</Card>
						</Grid>
					))}
				</Grid>
			</Box>

			<Box display="flex" flexDirection="column" alignItems="center" padding={2}>
				<Typography variant="caption" display="block" gutterBottom>
					© {new Date().getFullYear()} Intersect MBO
				</Typography>
				<Link href="https://sanchogov.tools/">
					<Typography variant="caption" display="block" sx={{ color: (theme) => theme?.palette?.text?.primaryBlue }}>
						Sancho Govtool
					</Typography>
				</Link>
			</Box>

		</Box >
	);
}

export default Dashboard;
