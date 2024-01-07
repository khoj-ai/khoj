import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'Connect your knowledge',
    Svg: require('@site/assets/img/mountains-lake.svg').default,
    description: (
      <>
        Khoj can understand your PDFs, markdown, org, and other plaintext files. Connect your knowledge base using one of our <a href="https://khoj.dev/downloads">desktop apps</a> or <a href="/docs/category/clients">other client apps</a>.
      </>
    ),
  },
  {
    title: 'Simplify your thinking',
    Svg: require('@site/assets/img/lightbulb-plant.svg').default,
    description: (
      <>
        Get your knowledge base out of your head and into Khoj. Khoj is a tool for thinking, learning, and making things.
      </>
    ),
  },
  {
    title: 'Run privately',
    Svg: require('@site/assets/img/folder_security.svg').default,
    description: (
      <>
        You can run Khoj on your own computer, or on your own server. All of our code is open source, and you can <a href="/docs/get-started/setup">set it up</a> in minutes.
      </>
    ),
  },
];

function Feature({Svg, title, description}) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
