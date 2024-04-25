'use client';

import { useAppContext } from '@/context/context';
import axiosInstance from '@/lib/axiosInstance';
import { Backdrop, CircularProgress } from '@mui/material';
import { useEffect } from 'react';

const Loader = () => {
	// const loaderRef = useRef(null);
	const { loading, setLoading } = useAppContext();
	let responseTimer;

	const openLoader = () => {
		// if (loaderRef.current) {
		// 	loaderRef.current.style.display = 'flex';
		// }
		setLoading(true);
	};

	const closeLoader = () => {
		// if (loaderRef.current) {
		// 	loaderRef.current.style.display = 'none';
		// }
		setLoading(false);
	};

	axiosInstance.interceptors.request.use(
		(config) => {
			openLoader();
			return config;
		},
		(error) => {
			closeLoader();
			return Promise.reject(error);
		}
	);

	axiosInstance.interceptors.response.use(
		(response) => {
			// clearTimeout(responseTimer);
			// responseTimer = setTimeout(() => {
			// 	closeLoader();
			// }, 500);
			closeLoader();
			return response;
		},
		(error) => {
			// clearTimeout(responseTimer);
			closeLoader();
			return Promise.reject(error);
		}
	);

	// useEffect(() => {
	// 	return () => {
	// 		clearTimeout(responseTimer);
	// 	};
	// }, []);

	useEffect(() => {
		if (loading) {
			openLoader();
		} else {
			closeLoader();
		}
	}, [loading]);

	return (
		// <div className="spinner-overlay" id="loader" ref={loaderRef}>
		// 	<div className="spinner-wrapper">
		// 		<SpinnerComponent
		// 			animation="border"
		// 			role="status"
		// 		></SpinnerComponent>
		// 	</div>
		// </div>
		// <Button onClick={handleOpen}>Show backdrop</Button>
		<Backdrop
			sx={{ color: '#fff', zIndex: (theme) => theme.zIndex.drawer + 100 }}
			open={loading}
			onClick={() => closeLoader()}
			// ref={loaderRef}
		>
			<CircularProgress size={100} color="inherit" />
		</Backdrop>
	);
};

export default Loader;
