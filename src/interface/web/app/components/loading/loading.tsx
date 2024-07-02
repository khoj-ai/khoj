import styles from './loading.module.css';

export default function Loading() {
    // return (
    //     <div className={`${styles.loading} h-[100vh] flex items-center justify-center`}>
    //         <button type="button" className="bg-indigo-500" disabled>
    //         Loading...
    //         <span className="relative flex h-3 w-3">
    //         <span className="animate-ping absolute inline-flex h-full w-full rounded-full bg-sky-400 opacity-75"></span>
    //         <span className="relative inline-flex rounded-full h-3 w-3 bg-sky-500"></span>
    //     </span>
    //     </button>
    //     </div>
    // )
    return (
        <div className={`${styles.loader} h-[100vh] flex items-center justify-center`}></div>
    );

    return (
        <div className="h-[100vh] flex items-center justify-center">
            <h2 className="text-4xl text-black animate-bounce">
                Loading...
            </h2>
        </div>
    )
}
