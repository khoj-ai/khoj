import styles from './chat.module.css';

import SuggestionCard from '../components/suggestionCard';

export default function Chat() {
    return (
        <div className={styles.main}>
            <div>
                hi, what do you want today?
            </div>
            <div className={styles.suggestions}>
                <SuggestionCard
                    title='I want to learn about React'
                    body='React is a JavaScript library for building user interfaces'
                    link='https://reactjs.org/'
                    styleClass='pink' />
            </div>
        </div>
    )
}
